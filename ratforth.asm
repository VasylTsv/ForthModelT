; Forth system for RatC virtual machine - Forth Model T (formerly known as RatForth)
; Part of Weekend Project Series by Vasyl Tsvirkunov
; A real Forth system built on a primitive CPU over the
; course of a few days to demonstrate that it is possible
; and to show the steps of bootstrapping the language on
; an unknown hardware.
; At this point the compliance status is:
; Forth-2012 System
; Providing the Core Extensions word set
; Providing names from the Programming-Tools word set
; Providing names from the String word set
; Providing the Double-Number word set
; Providing the File-Access word set 
; Providing names from the Facility word set
; 
; This system will pass all standard tests for Core, Core Extensions, Double-Number, and File Access.
; The supplied subset of Facility word set is sufficient to pass that test completely (only five words are tested).
; The partial Programming-Tools and String wordsets are compliant and will also pass individual tests.
; With dynamic-memory-allocation package by Ulrich Hoffmann installed the implementation will pass the
; Memory-Allocation tests (the implementation is slightly non-compliant and fails on negative sizes
; passed to ALLOCATE and RESIZE, those need to be patched for 100% clean test).
; Optional Block, Exception, Locals, and Search-Order sets are not implemented. No Floating-Point either.

start

; ==============================================================================
; Definitions

; The default RatC stack size is 16K which is an overkill for a system of that
; kind. Reusing half of that memory under return stack so both stacks together
; are 16K. It is still very generous
	equ #rstack			57344 		; 0xe000 or 64K-8K
	equ #stacklimit		4080 		; 8K bytes = 4K cells. 8 cells on either side for safe check
	equ #namemask		31			; vocabulary entries can have up to 32 characters
	equ #immflag		128			; flag for immediate words
	equ #nstflag		64			; flag for words not in ANS Forth
	equ #true			-1			; required by current standard
	equ #false			0
	
	equ #memtop			47104		; the memory between 46K and 47K is used for file buffers and other disk access services
	equ _ibuf			47104		; seven 100-char buffers for INCLUDE-FILE
	equ _sbuf			47804		; two 100-char buffers for S" / S\"
	equ _fnamebuf		48004		; buffer for filename storage only
	equ _sflip			48104		; flip-flop for S" buffer selection
	equ _ibufcount		48106		; number of used buffers
	
	equ _ri				48128		; the memory between 47K and 48K is used for internal variables and buffers
	equ _w				48130
	equ _rstack			48132

	equ _here			48134
	equ _base			48136
	equ _context		48138
	equ _latest			48140
	equ _state			48142
	equ _strict			48144
	equ _shigh			48146		; three "registers" for UM/MOD and M* (Shigh, Slow, and Sdiv, the last name is not very appropriate for M*)
	equ _slow			48148
	equ _sdiv			48150
	equ _scarry			48152		; register for carry as the VM does not have the flag
	equ _sstack			48154		; reference stack bottom
	
	equ _tib			48200		; input buffer (reserving 100 bytes although only 81 are really needed)
	equ _wordbuf		48300		; buffer to hold result of WORD (reserving 100 bytes)
	equ _findbuf		48400		; used by FIND to keep case-converted pattern word
	equ _pad			48500		; PAD buffer
	equ _hld			48600		; pointer for pictured numeric output, the 98-byte buffer follow
	equ _hldend			48700
	equ _source			48700		; pointer to the current source, source stack follows
	equ _sourcestack	48820		; end of the stack for sources (120 bytes to accomodate 7 files and default)
	equ #ramtop			49152		; stacks start here

; ==============================================================================

; To make the system more robust we need some way to recover from stack
; underruns and overruns. It cannot be 100% foolproof (the system with
; unrestricted POKE cannot be foolproof) but a single DROP should not
; take out the entire system.
; So, the approach is the following: reserve first 8 and last eight words
; of the stack and call ABORT if checks detect the stack there. Return stack
; may benefit from a similar treatment but it is much less likely to cause a
; problem

		push ; it is less code to just use 'push' 7 times than write a loop
		push ; note that the system already pushes one value on the stack
		push
		push
		push
		push
		push

		ldir.w xinit_startup	; init mutable system values (in high memory)
		sdir.w _ri				; _w does not need to be initialized
		limm #rstack
		sdir.w _rstack
		limm 10
		sdir.w _base
		ldir.w xinit_context
		sdir.w _context
		ldir.w xinit_latest
		sdir.w _latest
		ldir.w xinit_here
		sdir.w _here
		limm 0
		sdir.w _state
		sdir.w _strict
		sdir.w _sflip
		sdir.w _ibufcount
		addr 0
		sdir.w _sstack
		
; ==============================================================================

; Structure of a vocabulary word in indirect threaded code:
; offset     length      meaning
;    0          1        n - length of the name and flags (NFA)
;    1          n        name
;    n+1        2        link to the previous word (LFA)
;    n+3        2        pointer to code (typically CALL) (CFA)
;    n+5        x        parameter list (for CALL these often are pointers
;                          to code fields of other words (PFA)
; Note that the "CPU" architecture does not require alignment. Otherwise it would
; be easy to align LFA
; Example:
; Forth   : Example 2 dup + . ;
; Assembly:
; example_n: db 6, "Example"
;            db last_n
; example:   db call
;            db lit, 2, 0, dup, plus, dot, exit

; ==============================================================================
; Inner interpreter
;
; To start the interpret, RI needs to point to CFA of the first work to execute and NEXT should be executed

; NEXT - execute the word at RI (RI is pointer to CFA), prime the parameter pointer for CALL
;	W = mem(RI)
;	RI += 2
;	goto mem(W)

next:	ldir.w _ri
		push
		lind.w
		sdir.w _w
		lind.w
		xchange
		inc 2
		sdir.w _ri
		return

; CALL - this will execute the parameters at W
; 	rpush(RI)
;	RI = W+2
;	goto NEXT

call:	ldir.w _rstack
		swap
		ldir.w _ri
		sind.w
		swap
		dec 2
		sdir.w _rstack
		ldir.w _w
		inc 2
		sdir.w _ri
		ujump next

; EXIT - return from the current word to the caller (called "return" here to avoid conflict with EXIT word)
;	RI = rpop()
;	goto NEXT

return:	ldir.w _rstack
		inc 2
		sdir.w _rstack
		lind.w
		sdir.w _ri
		ujump next
		
; INVOKE - this will execute word by CFA and continue to the next word (exposed to the language as EXECUTE)
;	W = pop()
;	goto mem(W)

invoke:	pop
		swap
		sdir.w _w
		lind.w
		push
		return

; CREATED - push the PFA on the stack (default semantics of a word after CREATE)
;	push(W+2)
;	goto NEXT

created:	ldir.w _w
			inc 2
			push
			ujump next

; DOES - semantics applied to the defined word by DOES>. It is an extension of CREATE semantics that redirects
; the execution to the creating word. The CPU in use makes it more complex that it should really be
;	rpush(RI)
;	RI = pop()
;	push(W+2)
;	goto NEXT

does:	ldir.w _w		; push PFA on the stack
		inc 2
		xchange
		push			; need the return address still and we are out of registers
		ldir.w _rstack	; rpush(RI)
		swap
		ldir.w _ri
		sind.w
		swap
		dec 2
		sdir.w _rstack
		pop				; set RI to the return address from that call above, so to the next execution token after it
		swap
		sdir.w _ri
		ujump next

; DODEFER - semantics of the word created by DEFER
;	W = mem(W+2)
;	goto mem(W)

dodefer:	ldir.w _w
			inc 2
			lind.w
			sdir.w _w
			lind.w
			push
			return

; DOVALUE - semantics of a VALUE
;	Assumes a very particular structure: pointer to semantics block followed by value bytes. Semantics block contains
;	three addresses: read semantics, write semantics, compilation semantics
;	push(W+4)
;	W = mem(mem(W+2))
;	goto mem(W)

dovalue:	ldir.w _w
			inc 4
			push	; address of data bytes
			dec 2	; address of semantics block
			lind.w
			lind.w
			sdir.w _w
			lind.w	; read semantics
			push
			return

; A number of Forth words have constant semantics. Typical systems define CONSTANT using DOES> but that wastes a few
; bytes for the call. Using a separate semantic word instead.
;	push mem(W+2)
;	goto NEXT

doconst:	ldir.w _w
			inc 2
			lind.w
			push
			ujump next

; ==============================================================================
; This word became standard in ANS Forth, part of optional Programming-Tools word set. Quit the interpreter.

;
; code bye
; code exit
; code execute
; : quit (sst) ;code
; : abort begin depth 0> while drop again begin depth <0 while 0 again quit ;
;

bye_n:	db 3, "BYE"
		db 0, 0
bye:	db bye_c
bye_c:	end

; EXIT is used to return from any word.
exit_n: db 4, "EXIT"
		db bye_n
exit:   db return

; Execute the word by address on the stack
execute_n:	db 7, "EXECUTE"
			db exit_n
execute:	db invoke

; Reset return stack, dispose of sources, and reenter the system.
; Open question: should it close all open files?
quit_n:	db 4, "QUIT"
		db execute_n
quit:	db call, xsst, xquit
xquit:	db quit_c				; this is an equivalent to ;CODE
quit_c:	limm #rstack
		sdir.w _rstack
		limm forth_system_r		; don't show the banner
		sdir.w _ri
		ujump next

; Reset data stack and perform QUIT. There is no way directly reset the data stack in RatC VM
; but it can be done with push or pop in the loop
abort_n:	db 5, "ABORT"
			db quit_n
abort:		db call
abort_1:	db depth, zerogt, qbranch, abort_2, drop, branch, abort_1
abort_2:	db depth, zerolt, qbranch, abort_3, zero, branch, abort_2
abort_3:	db quit

;
; : (sst) _sourcestack
;         2- 0 over ! 2- _tib over ! 2- 0 over ! 2- 0 over ! 2- 4 over !
;         _source ! 0 dup _sflip ! _ibufcount ! ; nonstandard
;

xsst_n:	db 69, "(SST)"	; Reset source stack
		db abort_n
xsst:	db call, lit, _sourcestack
		db twominus, zero, over, poke			; #TIB
		db twominus, lit, _tib, over, poke		; TIB
		db twominus, zero, over, poke			; >IN
		db twominus, zero, over, poke			; SOURCE-ID
		db twominus, lit, 4, 0, over, poke		; standard input has 4 parameters: 0, >IN, TIB, #TIB
		db lit, _source, poke
		db zero, dup, lit, _sflip, poke, lit, _ibufcount, poke, exit

; Support for creating of a new executable binary. The (INITS) contains values that may change from the
; original binary (we intentionally don't modify anything below the original end of image). SAVE-SYSTEM
; first writes everything before the body of that word, then constructs a small buffer with different
; values, writes it out, and writes past that to HERE.
;
; code (inits) nonstandard
; : save-system parse-name w/o openfile 0= if
;               >r 0 $xinit_startup r@ write-file drop
;               pad $forth_system_c over !
;               2+ here over ! 2+ _context @ over ! 2+ _latest @ over !
;               drop pad 8 r@ write-file drop
;               $xinit_latest 2+ here over - r@ write-file drop r> close-file
;               then drop ; nonstandard
;

xinits_n:		db 71, "(INITS)"
				db xsst_n
xinits:			db created
xinit_startup:	db forth_system_c
xinit_here:		db end_of_image
xinit_context:	db forth_system_n
xinit_latest:	db forth_system_n

savesystem_n:	db 75, "SAVE-SYSTEM"
				db xinits_n
savesystem:		db call, parsename, wo, openfile, zeroeq, qbranch, savesystem_1
				db tor, zero, lit, xinit_startup, rat, writefile, drop
				db pad, lit, forth_system_c, over, poke
				db twoplus, here, over, poke
				db twoplus, lit, _context, peek, over, poke
				db twoplus, lit, _latest, peek, over, poke
				db drop, pad, lit, 8, 0, rat, writefile, drop
				db lit, xinit_latest, twoplus, here, over, sub, rat, writefile, drop
				db rfrom, closefile
savesystem_1:	db drop, exit

; ==============================================================================
; Integer math

; Adding special constants for small numbers that are used very often. This makes both interpretation
; and execution a little bit more efficient.
;
; 0 constant 0
; 1 constant 1
; 2 constant 2
; -1 constant -1
;

zero_n:	db 65, "0"
		db savesystem_n
zero:	db doconst, 0, 0

one_n:	db 65, "1"
		db zero_n
one:	db doconst, 1, 0

two_n:	db 65, "2"
		db one_n
two:	db doconst, 2, 0

minusone_n:	db 66, "-1"
			db two_n
minusone:	db doconst, 255, 255

; The alternative high-level implementations are particularly useful on
; platforms without native multiplication or division.
;
; code +
; code -
; code *        alt: : * m* drop ;
; code /        alt: : / /mod nip ;
; code mod      alt: : mod /mod drop ;
; code /mod     alt: : /mod >r s>d r> sm/rem ;
; code */mod    alt: : */mod >r m* r> sm/rem ;
; code */       alt: : */ */mod nip ;
;

add_n:	db 1, "+"
		db minusone_n
add:	db add_c
add_c:	pop
		swap
		pop
		add
		push
		ujump next
		
sub_n:	db 1, "-"
		db add_n
sub:	db sub_c
sub_c:	pop
		swap
		pop
		sub
		push
		ujump next
		
mult_n:	db 1, "*"
		db sub_n
; mult:	db call, mmult, drop, exit
mult:	db mult_c
mult_c:	pop
		swap
		pop
		mult
		push
		ujump next
		
div_n:	db 1, "/"
		db mult_n
; div:	db call, divmod, nip, exit
div:	db div_c
div_c:	pop
		swap
		pop
		div
		push
		ujump next
		
mod_n:	db 3, "MOD"
		db div_n
; mod:	db call, divmod, drop, exit
mod:	db mod_c
mod_c:	pop
		swap
		pop
		mod
		push
		ujump next
		
divmod_n:	db 4, "/MOD"
			db mod_n
; divmod:	db call, tor, stod, rfrom, smrem, exit
divmod:		db divmod_c
divmod_c:	pop
			swap
			pop
			mod
			push
			swap
			push
			ujump next

multdivmod_n:	db 5, "*/MOD"
				db divmod_n
multdivmod:		db call, tor, mmult, rfrom, smrem, exit

multdiv_n:	db 2, "*/"
			db multdivmod_n
multdiv:	db call multdivmod, nip, exit

;
; : abs dup 0< if negate then ;
; code negate
; code 1+		alt:	: 1+ 1 + ;
; code 1-		alt:	: 1- 1 - ;
; code 2+		alt:	: 2+ 2 + ;
; code 2-		alt:	: 2- 2 - ;
; code 2/		alt:	: 2/ 2 / ;
; code 2*		alt:	: 2* 2 * ;
;

abs_n:	db 3, "ABS"
		db multdiv_n
abs:	db call, dup, zerolt, qbranch, abs_1, negate
abs_1:	db exit

negate_n:	db 6, "NEGATE"
			db abs_n
negate:		db negate_c
negate_c:	pop
			swap
			neg
			push
			ujump next
		
oneplus_n:	db 2, "1+"
			db negate_n
; oneplus:	db call, one, add, exit
oneplus:	db oneplus_c
oneplus_c:	pop
			swap
			inc 1
			push
			ujump next
		
oneminus_n:	db 2, "1-"
			db oneplus_n
; oneminus:	db call, one, sub, exit
oneminus:	db oneminus_c
oneminus_c:	pop
			swap
			dec 1
			push
			ujump next
		
twoplus_n:	db 66, "2+"
			db oneminus_n
; twoplus:	db call, two, add, exit
twoplus:	db twoplus_c
twoplus_c:	pop
			swap
			inc 2
			push
			ujump next
		
twominus_n:	db 66, "2-"
			db twoplus_n
; twominus:	db call, two, sub, exit
twominus:	db twominus_c
twominus_c:	pop
			swap
			dec 2
			push
			ujump next
		
twodiv_n:	db 2, "2/"
			db twominus_n
twodiv:		db twodiv_c
twodiv_c:	pop
			limm 1
			asr
			push
			ujump next
			
twomult_n:	db 2, "2*"
			db twodiv_n
twomult:	db twomult_c
twomult_c:	pop
			limm 1
			asl
			push
			ujump next

;
; code lshift
; code rshift
;

lshift_n:	db 6, "LSHIFT"
			db twomult_n
lshift:		db lshift_c
lshift_c:	pop
			swap
			pop
			asl
			push
			ujump next
			
rshift_n:	db 6, "RSHIFT"
			db lshift_n
rshift:		db rshift_c
rshift_c:	pop
			swap
			pop
			fjump rshift_1	; shift by 0?
			dec 1
			push
			limm 1
			asr
			swap
			limm 32767
			and				; shift by 1 in P
			pop
			swap
			fjump rshift_1	; shift by 1?
			asr				; remaining shift
			swap
rshift_1:	swap
			push
			ujump next

; Double-word math follows. Some words are in Core even if they work with double values. Depending on architecture, this
; part may be relatively easy to do or quite hard. RatC VM is clearly at the very hard end when it comes to division and
; multiplication. However, there are only two words that really need to be implemented there. There are different models
; but the easiest is to start from UM/MOD and UM*.

;
; : s>d dup 0< if -1 else 0 then ;
; : dnegate invert swap invert swap one m+ ;
; : dabs dup 0< if dnegate then ;
;

stod_n:		db 3, "S>D"
			db rshift_n
stod:		db call, dup, zerolt, qbranch, stod_1, minusone, exit
stod_1:		db zero, exit

; Optional Double=number word set
dnegate_n:	db 7, "DNEGATE"
			db stod_n
dnegate:	db call, invert, swap, invert, swap, one, mplus, exit 

; Optional Double-numbler word set
dabs_n:		db 4, "DABS"
			db dnegate_n
dabs:		db call, dup, zerolt, qbranch, dabs_1, dnegate
dabs_1:		db exit

;
; : fm/mod dup >r sm/rem
;          over dup 0<> swap 0< r@ 0< xor and
;          if 1- swap r> + swap else rdrop then ;
;

fmmod_n:	db 6, "FM/MOD"
			db dabs_n
fmmod:		db call, dup, tor, smrem, over, dup, zerone, swap, zerolt, rat, zerolt, xor, and, qbranch, fmmod_1, oneminus, swap, rfrom, add, swap, branch, fmmod_2
fmmod_1:	db rdrop
fmmod_2:	db exit

;
; : sm/rem 2dup xor >r ( Sign of the quotient) over >r ( Sign of the remainder)
;          abs >r dabs r> um/mod
;          swap r> 0< if negate then
;          swap r> 0< if negate then ;
;

smrem_n:	db 6, "SM/REM"
			db fmmod_n
smrem:		db call, twodup, xor, tor, over, tor, abs, tor, dabs, rfrom, ummod
			db swap, rfrom, zerolt, qbranch, smrem_1, negate
smrem_1:	db swap, rfrom, zerolt, qbranch, smrem_2, negate
smrem_2:	db exit

;
; code um/mod
;
; This is the "shift dividend right" algorithm, something like this
;
;      dividend to (_shigh, _slow)
;      repeat (bits per cell) times
;          (_shigh, _slow) <<= 1      (*)
;          if divisor < _shigh        (*)
;              _shigh -= divisor
;              _slow++
;      _shigh to remainder
;      _slow to quotient
;
; (*) These two lines need to take in account the carry flag
;

; The CPU architecture is missing a lot of pieces needed to make this more efficient. I think it can be coded better on 6502.
; Tecnically, there may be a way to use the division from the VM but it may be about as messy and this algorithm may be useful
; on other architectures where division is not available or not usable for any other reason.
; Some Forth implementations go as far as having only UM/MOD and UM* in native code and defining all other division and
; multiplication through these words (see alternative implementation above).
ummod_n:	db 6, "UM/MOD"
			db smrem_n
ummod:		db ummod_c
ummod_c:	pop					; set up the registers
			swap
			sdir.w _sdiv
			pop
			swap
			sdir.w _shigh
			pop
			swap
			sdir.w _slow
			limm 16				; loop 16 times with loop counter on the stack
			push
ummod_1:	limm 0
			sdir.w _scarry
			ldir.w _slow
			call sub_lshift_carry
			sdir.w _slow
			ldir.w _shigh
			call sub_lshift_carry
			sdir.w _shigh
			swap
			ldir.w _sdiv
			testuge
			swap
			ldir.w _scarry
			or
			fjump ummod_3
			ldir.w _shigh
			swap
			ldir.w _sdiv
			sub
			sdir.w _shigh
			ldir.w _slow
			inc 1
			sdir.w _slow
ummod_3:	xchange
			dec 1				; end of the loop is here
			fjump ummod_4
			xchange
			ujump ummod_1
ummod_4:	pop					; push the results to the stack
			ldir.w _shigh
			push
			ldir.w _slow
			push
			ujump next

; One word left shift using carry and setting carry
sub_lshift_carry:	swap
					ldir.w _scarry
					push
					limm 32768
					and
					fjump slc_1
					limm 1
slc_1:				sdir.w _scarry
					limm 1
					asl
					pop
					or
					return

;
; : ud/mod >r 0 r@ um/mod rot rot r> um/mod rot ;
;

udmod_n:	db 70, "UD/MOD"
			db ummod_n
udmod:		db call, tor, zero, rat, ummod, rot, rot, rfrom, ummod, rot, exit

;
; code um*
;
; Another complex algorith, using "shift product right" version
;
;    multiplicand to (_shigh, _slow) (_shigh is 0 as multiplicand is only 16 bits)
;    repeat (bits per cell + 1) times
;        (_shigh, _slow) >>= 1
;        if (carry is set)
;            _shigh += multiplier
;    (_shigh, _slow) to product
;
; Again, don't forget the carry on addition that would apply to the next shift

ummult_n:	db 3, "UM*"
			db udmod_n
ummult:		db ummult_c
ummult_c:	pop				; set up "registers"
			swap
			sdir.w _sdiv
			pop
			swap
			sdir.w _slow
			limm 0
			sdir.w _shigh
			sdir.w _scarry
			limm 17			; set up the loop counter on the stack
			push
ummult_1:	ldir.w _shigh
			call sub_rshift_carry
			sdir.w _shigh
			ldir.w _slow
			call sub_rshift_carry
			sdir.w _slow
			ldir.w _scarry
			fjump ummult_2
			ldir.w _shigh
			swap
			ldir.w _sdiv
			call sub_add_carry
			sdir.w _shigh
ummult_2:	xchange			; end of loop
			dec 1
			fjump ummult_3
			xchange
			ujump ummult_1
ummult_3:	pop
			ldir.w _slow
			push
			ldir.w _shigh
			push
			ujump next

; One word right shift using carry and setting carry
sub_rshift_carry:	push
					ldir.w _scarry
					swap
					limm 15
					asl
					pop
					push
					limm 1
					and
					sdir.w _scarry
					limm 1
					asr
					swap
					limm 32767
					and
					pop
					or
					return

; Add _without_ carry but set carry based on the result
sub_add_carry:	add
				push
				testugt
				sdir.w _scarry
				pop
				swap
				return

; UD* is not part of the standard but it is very convenient to use in formatting words
;
; : m* 2dup xor >r abs swap abs um* r> 0< if dnegate then ;
; : ud* dup >r um* drop swap r> um* rot + ; nonstandard
;

mmult_n:	db 2, "M*"
			db ummult_n
mmult:		db call, twodup, xor, tor, abs, swap, abs, ummult, rfrom, zerolt, qbranch, mmult_1, dnegate
mmult_1:	db exit

udmult_n:	db 67, "UD*"
			db mmult_n
udmult:		db call, dup, tor, ummult, drop, swap, rfrom, ummult, rot, add, exit

;
; : m+ s>d d+ ;
;

; From the optional Double-number word set
mplus_n:	db 2, "M+"
			db udmult_n
mplus:		db call, stod, dadd, exit

; ==============================================================================
; Logical operations. Note that all operations are performed bitwise

;
; code and
; code or
; code xor
; : invert -1 xor ;
;

and_n:	db 3, "AND"
		db mplus_n
and:	db and_c
and_c:	pop
		swap
		pop
		and
		push
		ujump next
		
or_n:	db 2, "OR"
		db and_n
or:		db or_c
or_c:	pop
		swap
		pop
		or
		push
		ujump next
		
xor_n:	db 3, "XOR"
		db or_n
xor:	db xor_c
xor_c:	pop
		swap
		pop
		xor
		push
		ujump next

; Note that NOT has been removed from the standard.
invert_n:	db 6, "INVERT"
			db xor_n
invert:		db call, minusone, xor, exit

; ==============================================================================
; Comparisons

;
; code 0=
; code 0<
;

zeroeq_n:	db 2, "0="
			db invert_n
zeroeq:		db zeroeq_c
zeroeq_c:	pop
			limm 0
			testeq
			neg			; new standards require true to be -1, RatC VM uses 1
			push
			ujump next

zerolt_n:	db 2, "0<"
			db zeroeq_n
; zerolt:	db call, zero, less, exit
zerolt:		db zerolt_c
zerolt_c:	pop
			limm 0
			testlt
			neg
			push
			ujump next

;
;	: 0> 0 swap < ;
;	: 0<> 0= 0= ;
;	: = - 0= ;
;	: <> - 0<> ;
;

zerogt_n:	db 2, "0>"
			db zerolt_n
zerogt:		db call, zero, swap, less, exit
			
zerone_n:	db 3, "0<>"
			db zerogt_n
zerone:		db call, zeroeq, zeroeq, exit

equal_n:	db 1, "="
			db zerone_n
equal:		db call, sub, zeroeq, exit

notequal_n:	db 2, "<>"
			db equal_n
notequal:	db call, sub, zerone, exit

;
; code <
;

; Careful here. Some implementations have it as ": < - 0< ;" and it works... sometimes.
; Fails for "-32768 32767 <"
less_n:	db 1, "<"
		db notequal_n
less:	db less_c
less_c:	pop
		swap
		pop
		testlt
		neg
		push
		ujump next

;
;	: > swap < ;
;	: max 2dup < if swap then drop ;
;	: min 2dup > if swap then drop ;
;

greater_n:	db 1, ">"
			db less_n
greater:	db call, swap, less, exit

max_n:	db 3, "MAX"
		db greater_n
max:	db call, twodup, less, qbranch, max_1, swap
max_1:	db drop, exit

min_n:	db 3, "MIN"
		db max_n
min:	db call, twodup, greater, qbranch, min_1, swap
min_1:	db drop, exit

;
;	code u<
;

uless_n:	db 2, "U<"
			db min_n
uless:		db uless_c
uless_c:	pop
			swap
			pop
			testult
			neg
			push
			ujump next

;
;	: u> swap u< ;
;

ugreater_n:	db 2, "U>"
			db uless_n
ugreater:	db call, swap, uless, exit

;
;	-1 constant true
;	0 constant false
;

true_n:	db 4, "TRUE"
		db ugreater_n
true:	db doconst, #true

false_n:	db 5, "FALSE"
			db true_n
false:		db doconst, #false

;
;	: within over - >r - r> u< ;
;

within_n:	db 6, "WITHIN"
			db false_n
within:		db call, over, sub, tor, sub, rfrom, uless, exit

; ==============================================================================
; Base stack operations.

;
;	code dup
;	code drop
;	code over
;	code swap
;

dup_n:	db 3, "DUP"
		db within_n
dup:	db dup_c
dup_c:	pop
		swap
		push
		push
		ujump next
		
drop_n:	db 4, "DROP"
		db dup_n
drop:	db drop_c
drop_c:	pop
		ujump next
		
over_n:	db 4, "OVER"
		db drop_n
over:	db over_c
over_c:	pop
		swap
		pop
		swap
		push
		swap
		push
		swap
		push
		ujump next
		
swap_n:	db 4, "SWAP"
		db over_n
swap:	db swap_c
swap_c:	pop
		swap
		xchange
		push
		ujump next

;
;	: nip swap drop ;
;	: tuck swap over ;
;

nip_n:	db 3, "NIP"
		db swap_n
nip:	db call, swap, drop, exit

tuck_n:	db 4, "TUCK"
		db nip_n
tuck:	db call, swap, over, exit

;
; : rot >r swap r> swap ;
;

rot_n:	db 3, "ROT"
		db tuck_n
rot:	db call, tor, swap, rfrom, swap, exit

;
;	code pick
;	code roll
;

pick_n:	db 4, "PICK"
		db rot_n
pick:	db pick_c
pick_c:	pop
		limm 1
		asl
		swap
		addr 0
		add
		lind.w
		push
		ujump next

roll_n:	db 4, "ROLL"
		db pick_n
roll:	db roll_c
roll_c:	pop
		limm 1
		asl
		swap
		addr 0
		add
		push
		pop
		lind.w
		push
roll_1:	addr 2
		testne
		fjump roll_2
		limm 2
		sub
		lind.w
		sind.w
		swap
		dec 2
		swap
		ujump roll_1
roll_2:	pop
		swap
		pop
		push
		ujump next

;
;	code depth
;

; RatC VM does not have a good access to SP so this is a bit verbose. Could just use "addr 16"
; knowing that the stack is located at the end of the memory but this is more portable and will
; work with stack anywhere.
depth_n:	db 5, "DEPTH"
			db roll_n
depth:		db depth_c
depth_c:	ldir.w _sstack
			swap
			addr 0
			sub
			swap
			limm 1
			asr
			push
			ujump next

;
;	: 2drop drop drop ;
;	: 2dup over over ;
;	: 2swap >r rot rot >r rot rot ;
;	: 2over >r >r 2dup r> r> 2swap ;
;

twodrop_n:	db 5, "2DROP"
			db depth_n
twodrop:	db call, drop, drop, exit

twodup_n:	db 4, "2DUP"
			db twodrop_n
twodup:		db call, over, over, exit

twoswap_n:	db 5, "2SWAP"
			db twodup_n
twoswap:	db call, tor, rot, rot, rfrom, rot, rot, exit

twoover_n:	db 5, "2OVER"
			db twoswap_n
twoover:	db call, tor, tor, twodup, rfrom, rfrom, twoswap, exit

;
;	: ?dup dup if dup then ;
;

qdup_n:	db 4, "?DUP"
		db twoover_n
qdup:	db call, dup, qbranch, qdup_1, dup 
qdup_1: db exit

; ==============================================================================
; Standard cell/char size words and alignment (which do nothing on this architecture)

;
; : cell+ 2+ ;
; : cells dup + ;
; : char+ 1+ ;
; : chars ;
; : align ;
; : aligned ;
;

cellplus_n:	db 5, "CELL+"
			db qdup_n
cellplus:	db call, twoplus, exit
			
cells_n:	db 5, "CELLS"
			db cellplus_n
cells:		db call, dup, add, exit
			
charplus_n:	db 5, "CHAR+"
			db cells_n
charplus:	db call, oneplus, exit
			
chars_n:	db 5, "CHARS"
			db charplus_n
chars:		db call, exit	; that's correct, just do nothing

align_n:	db 5, "ALIGN"
			db chars_n
align:		db call, exit

aligned_n:	db 7, "ALIGNED"
			db align_n
aligned:	db call, exit

; ==============================================================================
; Words working with the return stack. These are probably among the most dangerous words in the language,
; any abuse would likely result in the system crash. An important aspect that all of these have to
; be implemented natively (don't try to implement RDROP as R> DROP - it won't work)

;
; code r>
; code >r
; code r@
; code rdrop nonstandard
; code 2>r
; code 2r>
; code 2r@
;

rfrom_n:	db 2, "R>"
			db aligned_n
rfrom:		db rfrom_c
rfrom_c:	ldir.w _rstack
			inc 2
			sdir.w _rstack
			lind.w
			push
			ujump next
			
tor_n:	db 2, ">R"
		db rfrom_n
tor:	db tor_c
tor_c:	ldir.w _rstack
		pop
		swap
		sind.w
		swap
		dec 2
		sdir.w _rstack
		ujump next
		
rat_n:	db 2, "R@"
		db tor_n
rat:	db rat_c
rat_c:	ldir.w _rstack
		inc 2
		lind.w
		push
		ujump next
		
rdrop_n:	db 69, "RDROP"
			db rat_n
rdrop:		db rdrop_c
rdrop_c:	ldir.w _rstack
			inc 2
			sdir.w _rstack
			ujump next

twotor_n:	db 3, "2>R"
			db rdrop_n
twotor:		db twotor_c
twotor_c:	pop
			swap
			pop
			push
			ldir.w _rstack
			swap
			sind.w
			swap
			dec 2
			pop
			swap
			sind.w
			swap
			dec 2
			sdir.w _rstack
			ujump next

tworfrom_n:	db 3, "2R>"
			db twotor_n
tworfrom:	db tworfrom_c
tworfrom_c:	ldir.w _rstack
			inc 4
			sdir.w _rstack
			lind.w
			push
			ldir.w _rstack
			dec 2
			lind.w
			push
			ujump next

tworat_n:	db 3, "2R@"
			db tworfrom_n
tworat:		db tworat_c
tworat_c:	ldir.w _rstack
			inc 4
			lind.w
			push
			ldir.w _rstack
			inc 2
			lind.w
			push
			ujump next

; ==============================================================================
; Basic memory operations

;
; code @
; code c@
; code !
; code c!
; : 2! swap over ! cell+ ! ;
; : 2@ dup cell+ @ swap peek ;
;

peek_n:	db 1, "@"
		db tworat_n
peek:	db peek_c
peek_c:	pop
		swap
		lind.w
		push
		ujump next

cpeek_n:	db 2, "C@"
			db peek_n
cpeek:		db cpeek_c
cpeek_c:	pop
			limm 255
			swap
			lind.b
			and
			push
			ujump next

poke_n:	db 1, "!"
		db cpeek_n
poke:	db poke_c
poke_c:	pop
		swap
		pop
		swap
		sind.w
		ujump next

cpoke_n:	db 2, "C!"
			db poke_n
cpoke:		db cpoke_c
cpoke_c:	pop
			swap
			pop
			swap
			sind.b
			ujump next

twopoke_n:	db 2, "2!"
			db cpoke_n
twopoke:	db call, swap, over, poke, cellplus, poke, exit

twopeek_n:	db 2, "2@"
			db twopoke_n
twopeek:	db call, dup, cellplus, peek, swap, peek, exit
			
; ==============================================================================
; Literal. One of the most common words to see in the compiled code - will take the next parameter and
; push it to stack

;
; code lit
; Alternative but slower: : lit r@ peek r> cell+ >r ; nonstandard
;

lit_n:	db 67, "LIT"
		db twopeek_n
; Could be done as a high level word but it makes sense to optimize this one for efficiency
;lit:	db call, rat, peek, rfrom, twoplus, tor, exit
lit:	db lit_c
lit_c:	ldir.w _ri
		lind.w
		push
		ldir.w _ri
		inc 2
		sdir.w _ri
		ujump next

; ==============================================================================
; Numeric output. Forth approach is a bit odd but extremely powerful

;
; variable base
; : <@ _hldend _hld ! ;
; : # base @ ud/mod rot '0' + dup '9' > if 7 + then hold ; 
; : #s begin # 2dup or 0= until ; 
; : #> 2drop _hld @ _hldend over - ;
; : hold _hld @ 1 - dup _hld ! c! ;
; : holds begin dup while 1- 2dup + c@ hold again 2drop ;
; : sign 0< if '-' hold then ;
;

base_n:	db 4, "BASE"
		db lit_n
base:	db doconst, _base

bhash_n:	db 2, "<#"
			db base_n
bhash:		db call, lit, _hldend, lit, _hld, poke, exit

hash_n:	db 1, "#"
		db bhash_n
hash:	db call, base, peek, udmod, rot, lit, 48, 0, add,
		db dup, lit, 57, 0, greater, qbranch, hash_1, lit, 7, 0, add
hash_1:	db hold, exit

hashs_n:	db 2, "#S"
			db hash_n
hashs:		db call
hashs_1:	db hash, twodup, or, zeroeq, qbranch, hashs_1, exit

hashb_n:	db 2, "#>"
			db hashs_n
hashb:		db call, twodrop, lit, _hld, peek, lit, _hldend, over, sub, exit

hold_n:	db 4, "HOLD"
		db hashb_n
hold:	db call, lit, _hld, peek, one, sub, dup, lit, _hld, poke, cpoke, exit

holds_n:	db 5, "HOLDS"
			db hold_n
holds:		db call
holds_1:	db dup, qbranch, holds_2, oneminus, twodup, add, cpeek, hold, branch, holds_1
holds_2:	db twodrop, exit

sign_n:	db 4, "SIGN"
		db holds_n
sign:	db call, zerolt, qbranch, sign_1, lit, 45, 0, hold
sign_1:	db exit

;
; : d.r >r dup >r dabs <# #s r> sign #> r> over - spaces type ;
; : d. 0 d.r space ;
; : .r swap s>d rot d.r ;
; : u. 0 d.
; : u.r 0 swap d.r ;
; : . s>d d. ;
;

ddotr_n:	db 3, "D.R"
			db sign_n
ddotr:		db call, tor, dup, tor, dabs, bhash, hashs, rfrom, sign, hashb, rfrom, over, sub, spaces, type, exit

ddot_n:		db 2, "D."
			db ddotr_n
ddot:		db call, zero, ddotr, space, exit

dotr_n:	db 2, ".R"
		db ddot_n
dotr:	db call, swap, stod, rot, ddotr, exit

udot_n:	db 2, "U."
		db dotr_n
udot:	db call, zero, ddot, exit

udotr_n:	db 3, "U.R"
			db udot_n
udotr:		db call, zero, swap, ddotr, exit

dot_n:	db 1, "."
		db udotr_n
dot:	db call, stod, ddot, exit

;
; : decimal 10 base ! ;
; : hex 16 base ! ;
;

decimal_n:	db 7, "DECIMAL"
			db dot_n
decimal:	db call, lit, 10, 0, base, poke, exit

hex_n:	db 3, "HEX"
		db decimal_n
hex:	db call, lit, 16, 0, base, poke, exit

; ==============================================================================
; HERE, comma, C,, etc.

;
; : +! dup @ rot + swap ! ;
; : here _here @ ;
; : allot _here +!
; : , here 2 allot ! ;
; : c, here 1 allot c! ;
;

incpoke_n:	db 2, "+!"
			db hex_n
incpoke:	db call, dup, peek, rot, add, swap, poke, exit

here_n:	db 4, "HERE"
		db incpoke_n
here:	db call, lit, _here, peek, exit

allot_n:	db 5, "ALLOT"
			db here_n
allot:		db call, lit, _here, incpoke, exit

comma_n:	db 1, ","
			db allot_n
comma:		db call, here, two, allot, poke, exit

ccomma_n:	db 2, "C,"
			db comma_n
ccomma:		db call, here, one, allot, cpoke, exit

; ==============================================================================
; Branching. All control strucutres are built on these two. The words are not in the standard
; but commonly used. ?BRANCH is sometimes named 0BRANCH
; There is an exotic way to implement BRANCH as
; : BRANCH R> @ >R ;
; This approach does not work for ?BRANCH due to chicken-and-egg problem

;
; code branch nonstandard
; code ?branch nonstandard
;

branch_n:	db 70, "BRANCH"
			db ccomma_n
branch:		db branch_c
branch_c:	ldir.w _ri
			lind.w
			sdir.w _ri
			ujump next

qbranch_n:	db 71, "?BRANCH"
			db branch_n
qbranch:	db qbranch_c
qbranch_c:	pop
			limm 0
			testne
			fjump branch_c
			ldir.w _ri
			inc 2
			sdir.w _ri
			ujump next

; ==============================================================================
; Line input support

tib_n:	db 67, "TIB"
		db qbranch_n
tib:	db call, lit, _source, peek, twoplus, twoplus, twoplus, peek, exit

ptrin_n:	db 3, ">IN"
			db tib_n
ptrin:		db call, lit, _source, peek, twoplus, twoplus, exit

numtib_n:	db 68, "#TIB"
			db ptrin_n
numtib:		db call, lit, _source, peek, twoplus, twoplus, twoplus, twoplus, exit

source_n:	db 6, "SOURCE"
			db numtib_n
source:		db call, tib, numtib, peek, exit

sourceid_n:	db 9, "SOURCE-ID"
			db source_n
sourceid:	db call, lit, _source, peek, twoplus, peek, exit

accept_n:	db 6, "ACCEPT"
			db sourceid_n
accept:		db accept_c
accept_c:	pop				; Cheating here, gets() is the only line input facility and it does not have a limiter
			call _gets		; But the actual implementation uses 80 which is compatible with this system
			pop
			swap
			push
accept_1:	push
			pop
			lind.b
			fjump accept_2
			swap
			inc 1
			ujump accept_1
accept_2:	swap
			pop
			swap
			sub
			push
			ujump next

key_n:		db 3, "KEY"
			db accept_n
key:		db key_c
key_c:		call _getch
			push
			ujump next

; : refill source-id 0< if false exit then
;          source-id 0= if cr ." Ok" cr tib 80 accept #tib ! 0 >in ! true exit then
;          source-id file-position drop _source 10 + 2!
;          tib 98 source-id read-line 0= and
;          if #tib ! 0 >in ! true exit then
;          drop false ;

; Note that slightly longer lines are being read from file - TODO: skip to the end of the line in files if buffer is full
refill_n:	db 6, "REFILL"
			db key_n
refill:		db call, sourceid, zerolt, qbranch, refill_1, false, exit	; "EVALUATE" - no refill
refill_1:	db sourceid, zeroeq, qbranch, refill_2, cr, lit, prompt, count, type, cr, tib, lit, 80, 0, accept, numtib, poke, zero, ptrin, poke, true, exit	; console
refill_2:	db sourceid, fileposition, drop, lit, _source, peek, lit, 10, 0, add, twopoke
			db tib, lit, 98, 0, sourceid, readline, zeroeq, and, qbranch, refill_3
			db numtib, poke, zero, ptrin, poke, true, exit	 ; file (note that the position is saved _before_ the refill)
refill_3:	db drop, false, exit
prompt:		db 2, "Ok"

;
; : save-input _source @ dup >r @ begin dup while dup 2* r@ + @ swap 1- again drop r> @ ;
;
; : restore-input over source-id = if
;                 source-id 0> if 6 pick 6 pick source-id reposition-file refill 2drop then
;                 begin dup while dup roll over 2* _source @ + ! 1- again drop false
;                 else true then ;
;

saveinput_n:	db 10, "SAVE-INPUT"
				db refill_n
saveinput:		db call, lit, _source, peek, dup, tor, peek
saveinput_1:	db dup, qbranch, saveinput_2, dup, twomult, rat, add, peek, swap, oneminus, branch, saveinput_1
saveinput_2:	db drop, rfrom, peek, exit

restoreinput_n:	db 13, "RESTORE-INPUT"
				db saveinput_n
restoreinput:	db call, over, sourceid, equal, qbranch, restoreinput_3
				db sourceid, zerogt, qbranch, restoreinput_1
				db lit, 6, 0, pick, lit, 6, 0, pick, sourceid, repositionfile, refill, twodrop
restoreinput_1:	db dup, qbranch, restoreinput_2, dup, roll, over, twomult, lit, _source, peek, add, poke, oneminus, branch, restoreinput_1
restoreinput_2:	db drop, false, exit
restoreinput_3:	db true, exit

ref saveinput
ref restoreinput

; ==============================================================================
; Some basic text output

emit_n:	db 4, "EMIT"
		db restoreinput_n
emit:	db emit_c
emit_c:	call _putchar
		modstk 2
		ujump next

cr_n:	db 2, "CR"
		db emit_n
cr:		db call, lit, 10, 0, emit, exit

bl_n:	db 2, "BL"
		db cr_n
bl:		db doconst, 32, 0

space_n:	db 5, "SPACE"
			db bl_n
space:		db call, bl, emit, exit

type_n:		db 4, "TYPE"
			db space_n
type:		db call
type_1:		db dup, zerone, qbranch, type_done
			db oneminus, swap, dup, cpeek, emit, oneplus, swap, branch, type_1
type_done:	db drop, drop, exit

count_n:	db 5, "COUNT"
			db type_n
count:		db call, dup, oneplus, swap, cpeek, exit

; Non-standard word but I can use this to make the system more friendly by ignoring case in FIND (CREATE needs to be aware as well)
toupper_n:	db 71, "TOUPPER"
			db count_n
toupper:	db call, swap, oneminus, swap
toupper_1:	db dup, zerone, qbranch, toupper_2, oneminus, swap, oneplus, swap, over, cpeek, dup
			db lit, 123, 0, less, swap, lit, 96, 0, greater, and, qbranch, toupper_1
			db swap, dup, cpeek, lit, 32, 0, sub, over, cpoke, swap, branch, toupper_1
toupper_2:	db drop, drop, exit

; ==============================================================================
; Word lookup. This is where the complex things begin.

word_n:	db 4, "WORD"
		db toupper_n
word:	db call, tor, source, swap, ptrin, peek, add
word_1:	db over, ptrin, peek, greater, qbranch, word_2
		db dup, cpeek, rat, equal, qbranch, word_2
		db ptrin, peek, oneplus, ptrin, poke, oneplus, branch, word_1
word_2:	db twodrop, rfrom, parse, dup, lit, _wordbuf, cpoke
		db lit, _wordbuf, oneplus, swap, cmove, lit, _wordbuf, exit

parse_n:		db 5, "PARSE"
				db word_n
parse:			db call, tor, source, ptrin, peek, sub, oneplus, tor, ptrin, peek, add, dup, zero
parse_1:		db over, cpeek, rfrom, oneminus, dup, qbranch, parse_3
				db swap, rat, equal, qbranch, parse_2
				db drop, swap, drop, rdrop, ptrin, dup, peek, oneplus, swap, poke, exit
parse_2:		db tor, swap, oneplus, swap, oneplus, ptrin, dup, peek, oneplus, swap, poke, branch, parse_1
parse_3:		db twodrop, swap, drop, rdrop, exit

parsename_n:	db 10, "PARSE-NAME"
				db parse_n
parsename:		db call, source, swap, ptrin, peek, add
parsename_1:	db over, ptrin, peek, greater, qbranch, parsename_2
				db dup, cpeek, bl, equal, qbranch, parsename_2
				db ptrin, peek, oneplus, ptrin, poke, oneplus, branch, parsename_1
parsename_2:	db twodrop, bl, parse, exit

; Forth systems typically have a few words to move between different parts of a vocabulary word. In the indirect
; threaded code the only non-trivial move is the one between LFA and NFA. In this particular model it abuses the
; fact that the maximum NFA length is 32+1 and the name cannot include characters with codes below 32. 
lfatonfa_n:	db 70, "L>NAME"
			db parsename_n
lfatonfa:	db call, oneminus, zero
lfatonfa_1:	db over, cpeek, lit, #namemask, and, over, notequal, qbranch, lfatonfa_2
			db swap, oneminus, swap, oneplus, dup, lit, 32, 0, equal, qbranch, lfatonfa_1
			db drop, drop, zero, exit
lfatonfa_2:	db drop, exit

tobody_n:	db 5, ">BODY"
			db lfatonfa_n
tobody:		db call, twoplus, exit

context_n:	db 71, "CONTEXT"
			db tobody_n
context:	db doconst, _context

latest_n:	db 70, "LATEST"
			db context_n
latest:		db call, lit, _latest, peek, exit

; COMPARE became standard in the later versions of the language. It makes writing FIND much easier
; In optional String word set
compare_n:	db 7, "COMPARE" ; (caddr1, u1, caddr2, u2 -> n)
			db latest_n
compare:	db call, swap, tor, tor, dup					; so the first string is on stack and the second is on rstack
compare_1:	db drop, dup, zeroeq, qbranch, compare_3		; dup/drop to make loop cleaner; check if reached the end of one of the strings
			db rat, zeroeq, qbranch, compare_lt				; * end of the first but not the second
			db branch, compare_eq							; * both ended
compare_3:	db rat, zeroeq, qbranch, compare_4
			db branch, compare_gt							; * end of the second but not the first
compare_4:	db oneminus, over, oneplus, swap, rot, cpeek	; step the first string and get one character on the stack
			db rfrom, rfrom, dup, cpeek, swap, oneplus, tor, swap, oneminus, tor ; same for the second one
			db sub, dup, zerone, qbranch, compare_1			; same character, continue
			db zerolt, qbranch, compare_gt					; either lesser or greater, done either way
compare_lt:	db drop, drop, rdrop, rdrop, minusone, exit
compare_eq:	db drop, drop, rdrop, rdrop, zero, exit
compare_gt:	db drop, drop, rdrop, rdrop, one, exit

strict_n:	db 70, "STRICT"
			db compare_n
strict:		db call, true, lit, _strict, poke, exit

; The only non-standard word that is visible in "strict" mode
extended_n:	db 8, "EXTENDED"
			db strict_n
extended:	db call, false, lit, _strict, poke, exit

;
; : (find) #namemask and dup >r _findbuf swap cmove _findbuf r@ toupper context peek
;          begin dup while count #namemask and 2dup
;          r@ = if r@ _findbuf r@ compare 0= if rdrop drop 1- exit then else drop then
;          + @ again rdrop ; nonstandard
;
; : find dup >r count (find) dup
;        if rdrop dup count #namemask and + 2+
;           swap c@ #immflag and -1 swap if negate then
;        else >r swap then ;
;

xfind_n:	db 70, "(FIND)"		; caddr, n -> NFA | 0
			db extended_n
xfind:		db call, lit, #namemask, and, dup, tor, lit, _findbuf, swap, cmove
			db lit, _findbuf, rat, toupper, context, peek
xfind_1:	db dup, qbranch, xfind_4, count, lit, #namemask, and, twodup, rat, equal
			db qbranch, xfind_2, rat, lit, _findbuf, rat, compare, zeroeq
			db qbranch, xfind_3, rdrop, drop, oneminus, exit
xfind_2:	db drop
xfind_3:	db add, peek, branch, xfind_1
xfind_4:	db rdrop, exit

find_n:		db 4, "FIND"
			db xfind_n
find:		db call, dup, tor, count, xfind, dup, qbranch, find_1
			db rdrop, dup, count, lit, #namemask, and, add, twoplus
			db swap, cpeek, lit, #immflag, and, minusone, swap, qbranch, find_2, negate, exit
find_1:		db rfrom, swap
find_2:		db exit

immediate_n:	db 9, "IMMEDIATE"
				db find_n
immediate:		db call, latest, dup, cpeek, lit, #immflag, or, swap, cpoke, exit

nonstandard_n:	db 75, "NONSTANDARD"
				db immediate_n
nonstandard:	db call, latest, dup, cpeek, lit, #nstflag, or, swap, cpoke, exit

xdigit_n:	db 71, "(DIGIT)"
			db nonstandard_n
xdigit:		db call, dup, lit, 48, 0, less, qbranch, xdigit_1
xdigit_x:	db drop, minusone, exit
xdigit_1:	db dup, lit, 58, 0, less, qbranch, xdigit_2, lit, 48, 0, sub, exit ; '0'-'9'
xdigit_2:	db dup, lit, 65, 0, less, qbranch, xdigit_3, branch, xdigit_x
xdigit_3:	db dup, lit, 91, 0, less, qbranch, xdigit_4, lit, 55, 0, sub, exit ; 'A'-'Z'
xdigit_4:	db dup, lit, 97, 0, less, qbranch, xdigit_5, branch, xdigit_x
xdigit_5:	db dup, lit, 123, 0, less, qbranch, xdigit_x, lit, 87, 0, sub, exit ; 'a'-'z'

tonumber_n:	db 7, ">NUMBER"
			db xdigit_n
tonumber:	db call
tonumber_1:	db dup, zerogt, qbranch, tonumber_3									; no more digits left?
			db over, cpeek, xdigit, dup, zerolt, zeroeq, qbranch, tonumber_2	; not a possible digit?
			db dup, base, peek, less, qbranch, tonumber_2						; not a digit in current base?
			db swap, oneminus, tor, swap, oneplus, tor, tor
			db base, peek, udmult, rfrom, mplus, rfrom, rfrom
			db branch, tonumber_1												; and repeat
tonumber_2:	db drop
tonumber_3:	db exit

number_n:		db 70, "NUMBER"
				db tonumber_n
number:			db call, count, base, peek, tor
				db dup, lit, 3, 0, equal, two, pick, cpeek, lit, 39, 0, equal, and			; character as 'c'
				db two, pick, two, add, cpeek, lit, 39, 0, equal, and, qbranch, number_8
				db drop, oneplus, cpeek, branch, number_5
number_8:		db dup, one, greater, qbranch, number_9
				db over, cpeek, lit, 35, 0, equal, qbranch, number_11, decimal, branch, number_10
number_11:		db over, cpeek, lit, 36, 0, equal, qbranch, number_12, hex, branch, number_10
number_12:		db over, cpeek, lit, 37, 0, equal, qbranch, number_9, two, base, poke
number_10:		db swap, oneplus, swap, oneminus
number_9:		db twodup, false, tor, over, cpeek, lit, 45, 0, equal, qbranch, number_1
				db rdrop, true, tor, oneminus, swap, oneplus, swap
number_1:		db zero, dup, twoswap, tonumber, dup, qbranch, number_4
				db one, equal, swap, cpeek, lit, 46, 0, equal, and, qbranch, number_7	; one unconverted char and it's '.'?
				db rfrom, qbranch, number_2, dnegate
number_2:		db twoswap, twodrop, state, peek, qbranch, number_3, compile, lit, swap, comma, compile, lit, comma
number_3:		db branch, number_6
number_4:		db twodrop, twoswap, twodrop, drop, rfrom, qbranch, number_5, negate
number_5:		db state, peek, qbranch, number_6, compile, lit, comma
number_6:		db rfrom, base, poke, exit
number_7:		db twodrop, type, xabortq, 2, " ?"


; ==============================================================================
; Nice service word that prints the entire list of supported words.

; In optional Programming-Tools word set
words_n:	db 5, "WORDS"
			db number_n
words:		db call, zero, context, peek
words_1:	db dup, count, dup, lit, #nstflag, and, lit, _strict, peek, and, swap
			db lit, #namemask, and, dup, tor, swap, zeroeq, qbranch, words_2
			db dup, zerone, qbranch, words_2
			db type, bl, emit, swap, oneplus, swap, branch, words_3
words_2:	db twodrop
words_3:	db rfrom, oneplus, add, peek, dup, zeroeq, qbranch, words_1
			db drop, cr, dot, lit, words_n, count, type, exit

ref words

; ==============================================================================
; Outer interpreter

state_n:	db 5, "STATE"
			db words_n
state:		db doconst, _state

qcomp_n:	db 69, "?COMP"
			db state_n
qcomp:		db call, state, peek, zeroeq, qbranch, qcomp_2
			db rat, twominus, twominus, dup, peek, lit, call, equal, qbranch, qcomp_1	; if ?COMP immediately follows CALL
			db twominus, lfatonfa, count, lit, #namemask, and, type, space					; output the word name with error
qcomp_1:	db xabortq, 20, "requires compilation"
qcomp_2:	db exit

qstack_n:	db 70, "?STACK"
			db qcomp_n
qstack:		db call, depth, dup, zerolt, swap, lit, #stacklimit, greater, or, qbranch, qstack_1
			db xabortq, 11, "Stack fault"
qstack_1:	db exit

interpret_n:	db 73, "INTERPRET"
				db qstack_n
interpret:		db call
interpret_1:	db qstack, bl, word, dup, cpeek, qbranch, interpret_done	; get the next word if any
				db state, peek, qbranch, interpret_int
				db find, dup, qbranch, comp_num
				db zerolt, qbranch, comp_imm		; compiling now
				db comma, branch, interpret_1		; regular word in compile mode
comp_imm:		db execute, branch, interpret_1		; immediate word in compile mode
comp_num:		db drop, number, branch, interpret_1
interpret_int:	db find, qbranch, int_num			; interpreting now
				db execute, branch, interpret_1		; any word in interpreter mode
int_num:		db number, branch, interpret_1
interpret_done:	db drop, refill, zeroeq, qbranch, interpret_1
				db closesource, exit

closesource_n:	db 76, "CLOSE-SOURCE"
				db interpret_n
closesource:	db call, sourceid, zerone, qbranch, closesource_2						; nothing to do with console source
				db sourceid, zerogt, qbranch, closesource_1
				db sourceid, closefile, drop, minusone, lit, _ibufcount, incpoke		; close file and release the buffer
closesource_1:	db lit, _source, dup, peek, dup, peek, oneplus, cells, add, swap, poke	; this will close the last frame
closesource_2:	db exit

evaluate_n:	db 8, "EVALUATE"
			db closesource_n
evaluate:	db call, lit, _source, peek
			db twominus, swap, over, poke
			db twominus, swap, over, poke
			db twominus, zero, over, poke
			db twominus, minusone, over, poke
			db twominus, lit, 4, 0, over, poke
			db lit, _source, poke
			db state, qbranch, evaluate_1, interpret
evaluate_1:	db exit

; ==============================================================================
; Colon definition and related words

xcreate_n:	db 72, "(CREATE)"
			db evaluate_n
xcreate:	db call, here, tor
			db dup, lit, 32, 0, greater, qbranch, xcreate_1, drop, lit, 31, 0 ; limit the word to 31 characters
xcreate_1:	db dup, ccomma, rat, oneplus, swap, dup, allot, cmove	; set NFA
			db rat, count, toupper
			db latest, comma										; set LFA
			db rfrom, lit, _latest, poke
			db lit, created, comma									; set CFA to a special code that pushes PFA on the stack
			db latest, context, poke, exit							; and add to the search order

create_n:	db 6, "CREATE"
			db xcreate_n
create:		db call, bl, word, count, xcreate, exit

; DOES> is a weird beast. It generates code that will modify the execution of the
; last defined word to jump to the definition word. It is also quite non-portable as it generates a low level instruction
xcode_n:	db 71, "(;CODE)"
			db create_n
xcode:		db call, rfrom										; which is the address of the "call xdoes" instruction
			db latest, count, lit, #namemask, and, add, twoplus	; CFA of the last defined word
			db poke, exit										; and this will actually exit the defining word

; Note that while DOES> looks like high-level word its implementation is depended on the opcode for native CALL/JSR
doesx_n:	db 133, "DOES>"
			db xcode_n
doesx:		db call, qcomp, compile, xcode, lit, 11, 0, ccomma, lit, does, comma, exit	; compile (;CODE) followed by "call does_c"

; Note that colon will remove the word from the search order (to be restored by semicolon)
colon_n:	db 1, ":"
			db doesx_n
colon:		db call, create, lit, call, here, twominus, dup, tor, poke, rfrom, twominus, peek, context, poke, bracketx, exit

; Words defined with :NONAME technically don't need to be linked in the vocabulary but if it is done that way RECURSE becomes harder
; to implement. It is easier just to link the word with emtpy name. In this implementation it has an unusual side effect that FIND
; will actually find the last :NONAME if searched for empty string and the test suite actually traps that (not an error though). But -
; standard does not specify it either way; and this is potentially useful.

;
; : :noname here 0 , latest , _latest ! here ' call , ] ;
;
 
colonnoname_n:	db 7, ":NONAME"
				db colon_n
colonnoname:	db call, here, zero, ccomma			; set 0-length NFA
				db latest, comma, lit, _latest, poke		; LFA
				db here, lit, call, comma, bracketx, exit	; CFA and keep the address on stack

bufferc_n:		db 7, "BUFFER:"
				db colonnoname_n
bufferc:		db call, create, allot, exit
				
semicolon_n:	db 129, ";"
				db bufferc_n
semicolon:		db call, qcomp, compile, exit, bracket, latest, context, poke, exit

variable_n:	db 8, "VARIABLE"
			db semicolon_n
variable:	db call, create, zero, comma, exit

twovariable_n:	db 9, "2VARIABLE"
				db variable_n
twovariable:	db call, create, zero, dup, comma, comma, exit

constant_n:	db 8, "CONSTANT"
			db twovariable_n
constant:	db call, create, comma, xcode, 11, does, peek, exit

defer_n:	db 5, "DEFER"
			db constant_n
defer:		db call, create, lit, dodefer, here, twominus, poke, compile, exit, exit

qdefer_n:	db 70, "?DEFER"
			db defer_n
qdefer:		db call, dup, peek, lit, dodefer, equal, qbranch, qdefer_1, exit
qdefer_1:	db xabortq, 11, "Not DEFERed"

deferpeek_n:	db 6, "DEFER@"
				db qdefer_n
deferpeek:		db call, qdefer, tobody, peek, exit

deferpoke_n:	db 6, "DEFER!"
				db deferpeek_n
deferpoke:		db call, qdefer, tobody, poke, exit

actionof_n:		db 137, "ACTION-OF"
				db deferpoke_n
actionof:		db call, state, peek, qbranch, actionof_1, btick, compile, deferpeek, exit
actionof_1:		db tick, deferpeek, exit

is_n:	db 130, "IS"
		db actionof_n
is:		db call, state, peek, qbranch, is_1, btick, compile, deferpoke, exit
is_1:	db tick, deferpoke, exit

;
; : (comp!) compile lit , compile ! ; nonstandard
; : value create ' dovalue here 2- ! ' >mark , , exit >resolve @ ! (comp!) ;
;

value_n:	db 5, "VALUE"
			db is_n
value:		db call, create, lit, dovalue, here, twominus, poke, lit, value_sem, comma, comma, exit
value_sem:	db peek, poke, comppoke
comppoke:	db call, compile, lit, comma, compile, poke, exit

to_n:		db 130, "TO"
			db value_n
to:			db call, bl, word, find, drop, dup, peek, lit, dovalue, equal, qbranch, to_2
			db twoplus, dup, twoplus, swap, peek, state, peek, qbranch, to_1, twoplus
to_1:		db twoplus, peek, execute, exit
to_2:		db xabortq, 11, "Not a VALUE"

tick_n:		db 1, "'"
			db to_n
tick:		db call, bl, word, find, drop, exit

btick_n:	db 131, "[']"
			db tick_n
btick:		db call, qcomp, bl, word, find, drop, compile, lit, comma, exit

; This will get the next parameter, compile it to the current definition and skip
compile_n:	db 71, "COMPILE"
			db btick_n
compile:	db call, rfrom, dup, twoplus, tor, peek, comma, exit

bcompile_n:	db 137, "[COMPILE]"
			db compile_n
bcompile:	db call, qcomp, tick, comma, exit

; Somehow I've managed to get this to pass the tests but I still don't completely understand what
; it is supposed to do
postpone_n:	db 136, "POSTPONE"
			db bcompile_n
postpone:	db call, qcomp, bl, word, find, one, equal, qbranch, postpone_1, comma, exit
postpone_1:	db lit, compile, comma, comma, exit

; This word behaves differently depending on compilation state - in compilation it
; will emit LIT followed by the value from the stack
literal_n:	db 135, "LITERAL"
			db postpone_n
literal:	db call, qcomp, state, peek, qbranch, literal_1, compile, lit, comma
literal_1:	db exit

bracket_n:	db 129, "["
			db literal_n
bracket:	db call, qcomp, false, state, poke, exit

bracketx_n:	db 1, "]"
			db bracket_n
bracketx:	db call, true, state, poke, exit

;
; : ( source-id 0< if
;     begin ')' parse 2drop >in @ #tib @ = tib #tib @ + 1- c@ ')' = and
;     while refill invert if exit then again
;     else ')' parse 2drop then ; immediate
;

brace_n:	db 129, "("
			db bracketx_n
brace:		db call, sourceid, zerogt, qbranch, brace_2
brace_1:	db lit, 41, 0, parse, twodrop, ptrin, peek, numtib, peek, equal, tib, numtib, peek, add, oneminus, cpeek, lit, 41, 0, notequal, and
			db qbranch, brace_3, refill, invert, qbranch, brace_1, exit
brace_2:	db lit, 41, 0, parse, twodrop
brace_3:	db exit

backslash_n:	db 129, "\"
				db brace_n
backslash:		db call, zero, parse, twodrop, exit

dotbrace_n:	db 130, ".("
			db backslash_n
dotbrace:	db call, lit, 41, 0, parse, type, exit

commaquote_n:	db 66, ',"'
				db dotbrace_n
commaquote:		db call, lit, 34, 0, parse, dup, ccomma, here, over, allot, swap, cmove, exit

cquote_n:	db 130, 'C"'
			db commaquote_n
cquote:		db call, qcomp, compile, branch, fmark, here, swap, commaquote, fresolve, compile, lit, comma, exit

squote_n:	db 130, 'S"'
			db cquote_n
squote:		db call, state, peek, zerone, qbranch, squote_1, cquote, compile, count, exit
squote_1:	db lit, 34, 0, parse, lit, _sbuf, lit, _sflip, peek, lit, 100, 0, mult, add, swap, twotor, tworat, cmove
			db tworfrom, lit, _sflip, dup, peek, one, xor, swap, poke, exit 

ssquote_n:	db 131, 'S\"'
			db squote_n
ssquote:	db call, tib, ptrin, peek, add, lit, _sbuf, lit, _sflip, peek, lit, 100, 0, mult, add, numtib, peek, ptrin, peek, sub, over, tor
			db smove, swap, ptrin, incpoke
			db rfrom, swap, lit, _sflip, dup, peek, one, xor, swap, poke
			db state, peek, qbranch, ssquote_1, compile, branch, fmark, here, two, pick, dup, ccomma, allot, swap, fresolve
			db compile, lit, dup, comma, compile, count, oneplus, swap, cmove 
ssquote_1:	db exit

dotquote_n:	db 130, '."'
			db ssquote_n
dotquote:	db call, qcomp, cquote, compile, count, compile, type, exit

compilecomma_n:	db 8, "COMPILE,"
				db dotquote_n
compilecomma:	db call, comma, exit

char_n:	db 4, "CHAR"
		db compilecomma_n
char:	db call, bl, word, charplus, cpeek, exit

bcharb_n:	db 134, "[CHAR]"
			db char_n
bcharb:		db call, compile, lit, bl, word, charplus, cpeek, comma, exit

xabortq_n:	db 72, '(ABORT")'
			db bcharb_n
xabortq:	db call, rat, count, type, abort, exit

abortq_n:	db 134, 'ABORT"'
			db xabortq_n
abortq:		db call, qcomp, compile, qbranch, fmark, compile, xabortq, commaquote, fresolve, exit

; In optional Programming-Tools word set
forget_n:	db 6, "FORGET"
			db abortq_n
forget:		db call, bl, word, find, zerone, qbranch, forget_1
			db twominus, dup, peek, dup, lit, _latest, poke, context, poke,
			db lfatonfa, lit, _here, poke, exit
forget_1:	db drop, exit

marker_n:	db 6, "MARKER"
			db forget_n
marker:		db call, create, xcode, 11, does
			db twominus, twominus, dup, peek, dup, lit, _latest, poke, context, poke,
			db lfatonfa, lit, _here, poke, exit

recurse_n:	db 135, "RECURSE"
			db marker_n
recurse:	db call, qcomp, latest, count, add, cellplus, comma, exit

; ==============================================================================
; More control structure support

fmark_n:	db 69, ">MARK"
			db recurse_n
fmark:		db call, here, zero, comma, exit

fresolve_n:	db 72, ">RESOLVE"
			db fmark_n
fresolve:	db call, here, swap, poke, exit

rmark_n:	db 69, "<MARK"
			db fresolve_n
rmark:		db call, here, exit

rresolve_n:	db 72, "<RESOLVE"
			db rmark_n
rresolve:	db call, comma, exit

; ==============================================================================
; Control words. All of these are immediate and make don't do anything useful
; in interpreter mode. There should be no code calling to CFA of these words,
; the CFA labels are removed to enforce that
; To understand the concept behind these words look at the BEGIN/AGAIN pair -
; BEGIN ends up just putting RI on the stack and AGAIN compiles BRANCH to that RI.
; Forward references are a bit trickier but follow the same pattern.

begin_n:	db 133, "BEGIN"
			db rresolve_n
			db call, qcomp, rmark, exit

until_n:	db 133, "UNTIL"
			db begin_n
			db call, qcomp, compile, qbranch, rresolve, exit

again_n:	db 133, "AGAIN"
			db until_n
			db call, qcomp, compile, branch, rresolve, exit

if_n:	db 130, "IF"
		db again_n
		db call, qcomp, compile, qbranch, fmark, exit

then_n:	db 132, "THEN"
		db if_n
		db call, qcomp, fresolve, exit

else_n:	db 132, "ELSE"
		db then_n
		db call, qcomp, compile, branch, fmark, swap, fresolve, exit

while_n:	db 133, "WHILE"
			db else_n
			db call, qcomp, compile, qbranch, fmark, swap, exit

repeat_n:	db 134, "REPEAT"
			db while_n
			db call, qcomp, compile, branch, rresolve, fresolve, exit

; DO/LOOP are considerably more complex so they are often implemented with helper words. In practical
; implementations these helpers are done in native code, so are I, J, and LEAVE.
; (DO) stores on the return stack: leaveaddr, limit, current, (ret) 
xdo_n:		db 68, "(DO)"
			db repeat_n
xdo:		db call
			db rfrom, dup, peek, tor	; forward ref for LEAVE
			db rot, tor, swap, tor,
			db twoplus, tor				; step over the actual forward ref
			db exit

xqdo_n:		db 69, "(?DO)"
			db xdo_n
xqdo:		db call
			db twodup, equal, qbranch, xqdo_1, twodrop, rfrom, peek, tor, exit
xqdo_1:		db rfrom, dup, peek, tor	; forward ref for LEAVE
			db rot, tor, swap, tor,
			db twoplus, tor				; step over the actual forward ref
			db exit
			
; and (LOOP) adjusts the values on rstack or just drops the top three values from it to exit
xloop_n:	db 70, "(LOOP)"
			db xqdo_n
xloop:		db call, rfrom				; return address is only needed to get the backref
			db rfrom, oneplus			; new value of current
			db rat, over, equal, qbranch, xloop_1
			db drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xloop_1:	db tor, peek, tor, exit		; continue the loop

xploop_n:	db 71, "(+LOOP)"
			db xloop_n
xploop:		db call, rfrom, swap		; return address is only needed to get the backref / addr, step
			db dup, rat, add			; preserve step value and get new value of current / addr, step, newcur
			db rfrom, rat, sub			; diff limit and new current / addr, step, newcur, olddiff
			db rot, over, xor, zerolt, swap ; new diff and step have different signs? / addr, newcur, step^olddiff<0, olddiff
			db two, pick, rat, sub		; diff limit and previous current / addr, newcur, s^d, olddiff, newdiff
			db xor, zerolt, and, qbranch, xploop_1  ; or diffs before and after have different signs / newdiff^olddiff < 0
			db drop, drop, rdrop, exit	; exit the loop (leaveaddr on the rstack)
xploop_1:	db tor, peek, tor, exit		; continue the loop

do_n:	db 130, "DO"
		db xploop_n
do:		db call, qcomp, compile, xdo, fmark, rmark, exit

qdo_n:	db 131, "?DO"
		db do_n
qdo:	db call, qcomp, compile, xqdo, fmark, rmark, exit

loop_n:	db 132, "LOOP"
		db qdo_n
loop:	db call, qcomp, compile, xloop, rresolve, fresolve, exit

ploop_n:	db 133, "+LOOP"
			db loop_n
ploop:		db call, qcomp, compile, xploop, rresolve, fresolve, exit

i_n:	db 1, "I"
		db ploop_n
i:		db call, rfrom, rat, swap, tor, exit

j_n:	db 1, "J"
		db i_n
j:		db call
		db rfrom, rfrom, rfrom, rfrom, rfrom, dup, tor, swap, tor, swap, tor, swap, tor, swap, tor, exit

leave_n:	db 5, "LEAVE"
			db j_n
leave:		db call, rdrop, rdrop, rdrop, exit
			
unloop_n:	db 6, "UNLOOP"
			db leave_n
unloop:		db call, rfrom, rdrop, rdrop, rdrop, tor, exit

case_n:	db 132, "CASE"
		db unloop_n
case:	db call, qcomp, depth, rfrom, swap, tor, tor, exit

of_n:	db 130, "OF"
		db case_n
of:		db call, qcomp, compile, over, compile, equal, compile, qbranch, fmark, compile, drop, exit

endof_n:	db 133, "ENDOF"
			db of_n
endof:		db call, qcomp, compile, branch, fmark, swap, fresolve, exit

endcase_n:	db 135, "ENDCASE"
			db endof_n
endcase:	db call, qcomp, compile, drop, depth, rfrom, rfrom, swap, tor, sub
endcase_1:	db dup, qbranch, endcase_2, oneminus, swap, fresolve, branch, endcase_1
endcase_2:	db drop, exit

; ==============================================================================
; Some nice to have words

spaces_n:	db 6, "SPACES"
			db endcase_n
spaces:		db call
spaces_1:	db dup, zerogt, qbranch, spaces_2, oneminus, space, branch, spaces_1
spaces_2:	db drop, exit

; In optional String word set
cmove_n:	db 5, "CMOVE"
			db spaces_n
cmove:		db call, rot, tor, swap
cmove_1:	db over, zerone, qbranch, cmove_done
			db rfrom, dup, cpeek, swap, oneplus, tor
			db over, cpoke, oneplus, swap, oneminus, swap, branch, cmove_1
cmove_done:	db drop, drop, rdrop, exit

; In optional String word set
cmovex_n:		db 6, "CMOVE>"
				db cmove_n
cmovex:			db call, rot, over, oneminus, add, tor, swap, over, oneminus, add
cmovex_1:		db over, zerone, qbranch, cmovex_done
				db rfrom, dup, cpeek, swap, oneminus, tor
				db over, cpoke, oneminus, swap, oneminus, swap, branch, cmovex_1
cmovex_done:	db drop, drop, rdrop, exit

move_n:	db 4, "MOVE"
		db cmovex_n
move:	db call, rot, rot, twodup, less, qbranch, move_1, rot, cmovex, exit
move_1:	db rot, cmove, exit

; Non-standard word, similar to CMOVE but does character conversions for S\". Returns number
; of characters processed and returned
smove_n:	db 69, "SMOVE"
			db move_n
smove:		db call, dup, tor, base, peek, tor, zero, tor
smove_1:	db dup, qbranch, smove_x, oneminus, two, pick, cpeek, lit, '"', 0, notequal, qbranch, smove_x ; decrement before check for quote is intentional!
			db tor, tor, dup, oneplus, swap, cpeek, dup, lit, "\", 0, equal, qbranch, smove_0
			db drop, rfrom, rfrom, dup, qbranch, smove_x, oneminus
			db tor, tor, dup, oneplus, swap, cpeek
			db dup, lit, "a", 0, equal, qbranch, smove_2, drop, lit, 7, 0, branch, smove_0
smove_2:	db dup, lit, "b", 0, equal, qbranch, smove_3, drop, lit, 8, 0, branch, smove_0
smove_3:	db dup, lit, "e", 0, equal, qbranch, smove_4, drop, lit, 27, 0, branch, smove_0
smove_4:	db dup, lit, "f", 0, equal, qbranch, smove_5, drop, lit, 12, 0, branch, smove_0
smove_5:	db dup, lit, "l", 0, equal, qbranch, smove_6, drop, lit, 10, 0, branch, smove_0
smove_6:	db dup, lit, "n", 0, equal, qbranch, smove_7, drop, lit, 10, 0, branch, smove_0
smove_7:	db dup, lit, "q", 0, equal, qbranch, smove_8, drop, lit, 34, 0, branch, smove_0
smove_8:	db dup, lit, "r", 0, equal, qbranch, smove_9, drop, lit, 13, 0, branch, smove_0
smove_9:	db dup, lit, "t", 0, equal, qbranch, smove_10, drop, lit, 9, 0, branch, smove_0
smove_10:	db dup, lit, "v", 0, equal, qbranch, smove_11, drop, lit, 11, 0, branch, smove_0
smove_11:	db dup, lit, "z", 0, equal, qbranch, smove_12, drop, zero, branch, smove_0
smove_12:	db dup, lit, "m", 0, equal, qbranch, smove_13, drop
			db rfrom, lit, 13, 0, over, cpoke, oneplus, lit, 10, 0, over, cpoke, oneplus,
			db rfrom, rfrom, twoplus, tor, branch, smove_1
smove_13:	db dup, lit, "x", 0, equal, qbranch, smove_0, drop
			db rfrom, rfrom, dup, one, greater, qbranch, smove_x, twominus, tor, tor
			db hex, dup, twoplus, swap, zero, zero, rot, two, tonumber, qbranch, smove_14
			db twodrop, twodrop, rdrop, rdrop, rfrom, rfrom, base, poke, exit
smove_14:	db twodrop
smove_0:	db rfrom, swap, over, cpoke, oneplus, rfrom, rfrom, oneplus, tor, branch, smove_1 
smove_x:	db nip, nip, rfrom, rfrom, base, poke, rfrom, rot, sub, swap, exit

pad_n:	db 3, "PAD"
		db smove_n
pad:	db doconst, _pad

unused_n:	db 6, "UNUSED"
			db pad_n
unused:		db call, lit, #memtop, here, sub, exit

fill_n:		db 4, "FILL"
			db unused_n
fill:		db call, swap, tor, swap
fill_1:		db rfrom, dup, qbranch, fill_2, oneminus, tor, twodup, cpoke, oneplus, branch, fill_1
fill_2:		db twodrop, drop, exit

erase_n:	db 5, "ERASE"
			db fill_n
erase:		db call, zero, fill, exit

sstring_n:	db 7, "/STRING"
			db erase_n
sstring:	db call, rot, over, add, rot, rot, sub, exit

blank_n:	db 5, "BLANK"
			db sstring_n
blank:		db call, bl, fill, exit

sliteral_n:	db 136, "SLITERAL"
			db blank_n
sliteral:	db call, state, peek, qbranch, sliteral_1, compile, branch, fmark, rot, rot
			db dup, tor, here, dup, tor, swap, dup, allot, cmove, fresolve
			db compile, lit, rfrom, comma, compile, lit, rfrom, comma
sliteral_1:	db exit

qmark_n:	db 1, "?"
			db sliteral_n
qmark:		db call, peek, dot, exit

dots_n:		db 2, ".S"
			db qmark_n
dots:		db call, depth
dots_1:		db dup, qbranch, dots_2, dup, pick, dot, oneminus, branch, dots_1
dots_2:		db drop, exit

ahead_n:	db 5, "AHEAD"
			db dots_n
ahead:		db call, fmark, exit

; ==============================================================================
; More words from the optional Double-Number word set

;
; : d= rot = >r = r> and ;
;

dequal_n:	db 2, "D="
			db ahead_n
dequal:		db call, rot, equal, tor, equal, rfrom, and, exit

;
; : dmax 2over 2over d< if 2swap then 2drop ;
; : dmin 2over 2over d< invert if 2swap then 2drop ;
;

dmax_n:	db 4, "DMAX"
		db dequal_n
dmax:	db call, twoover, twoover, dless, qbranch, dmax_1, twoswap
dmax_1: db twodrop, exit

dmin_n:	db 4, "DMIN"
		db dmax_n
dmin:	db call, twoover, twoover, dless, invert, qbranch, dmin_1, twoswap
dmin_1:	db twodrop, exit

;
; : d- dnegate d+ ;
; : d+ >r swap >r dup >r + dup r> u< 1 and r> r> + + ;
;

dsub_n:	db 2, "D-"
		db dmin_n
dsub:	db call, dnegate, dadd, exit

dadd_n:	db 2, "D+"
		db dsub_n
dadd:	db call, tor, swap, tor, dup, tor, add, dup, rfrom, uless, one, and, rfrom, rfrom, add, add, exit

dtwodiv_n:	db 3, "D2/"
			db dadd_n
dtwodiv:	db call, dup, one, and, lit, 15, 0, lshift, swap, twodiv, swap, rot, twodiv, or, swap, exit

;
; : d2* 2dup d+ ;
;

dtwomul_n:	db 3, "D2*"
			db dtwodiv_n
dtwomul:	db call, twodup, dadd, exit

duless_n:	db 3, "DU<"
			db dtwomul_n
duless:		db call, rot, twodup, equal, qbranch, duless_1, twodrop, uless, exit
duless_1:	db ugreater, qbranch, duless_2, twodrop, true, exit
duless_2:	db twodrop, false, exit

;
; : d0= or 0= ;
; : d0< nip 0< ;
; : d< rot > if 2drop true else < then ;
;

dzeroeq_n:	db 3, "D0="
			db duless_n
dzeroeq:	db call, or, zeroeq, exit

dzeroless_n:	db 3, "D0<"
				db dzeroeq_n
dzeroless:		db call, nip, zerolt, exit

dless_n:	db 2, "D<"
			db dzeroless_n
dless:		db call, rot, twodup, equal, qbranch, dless_1, twodrop, uless, exit
dless_1:	db greater, qbranch, dless_2, twodrop, true, exit
dless_2:	db twodrop, false, exit

;
; : d>s drop ;
;

dtos_n:	db 3, "D>S"
		db dless_n
dtos:	db call, drop, exit

;
; : 2constant create , , does> 2@ ;
;

dconstant_n:	db 9, "2CONSTANT"
				db dtos_n
dconstant:		db call, create, comma, comma, xcode, 11, does, twopeek, exit

;
; : 2lit r@ 2@ r> 2+ 2+ >r ; nonstandard
; : 2literal ?comp state @ if compile 2lit , , then ; immediate
;

dlit_n:	db 68, "2LIT"
		db dconstant_n
dlit:	db call, rat, twopeek, rfrom, twoplus, twoplus, tor, exit

dliteral_n:	db 136, "2LITERAL"
			db dlit_n
dliteral:	db call, qcomp, state, peek, qbranch, dliteral_1, compile, dlit, comma, comma
dliteral_1:	db exit

;
; : 2rot 5 roll 5 roll ;
;

drot_n:	db 4, "2ROT"
		db dliteral_n
drot:	db call, lit, 5, 0, roll, lit, 5, 0, roll, exit

dvalue_n:		db 6, "2VALUE"
				db drot_n
dvalue:			db call, create, lit, dovalue, here, twominus, poke, lit, dvalue_sem, comma, comma, comma, exit
dvalue_sem:		db twopeek, twopoke, compdpoke
compdpoke:		db call, compile, lit, comma, compile, twopoke, exit

;
; M*/ is an unusual word that uses three-cell numbers. It is possible to build it from the existing words
; To make it more clear, using some internal helpers:
; : t* ( ud,u -- ut) 2>r r@ m* 0 2r> m* d+ ;
; : t/ ( ut,u -- ud) dup >r um/mod r> swap >r um/mod nip r> ;
; : normsign ( d,n -- ud,u,n ) 2dup xor >r abs rot rot dabs rot r> ;
;
tmult:	db call, twotor, rat, ummult, zero, tworfrom, ummult, dadd, exit
tdiv:	db call, dup, tor, ummod, rfrom, swap, tor, ummod, nip, rfrom, exit
normsign:	db call, twodup, xor, tor, abs, rot, rot, dabs, rot, rfrom, exit

;
; : m*/ >r normsign r> swap >r >r t* r> t/ r> 0< if dnegate then ;
;

mmuldiv_n:	db 3, "M*/"
			db dvalue_n
mmuldiv:	db call, tor, normsign, rfrom, swap, tor, tor, tmult, rfrom, tdiv, rfrom, zerolt, qbranch, mmuldiv_1, dnegate
mmuldiv_1:	db exit

; ==============================================================================

env_counted_n:	db 79, "/COUNTED-STRING"
				db mmuldiv_n
				db doconst, 255, 0
				
env_hold_n:		db 69, "/HOLD"
				db env_counted_n
				db doconst, 98, 0
				
env_pad_n:		db 68, "/PAD"
				db env_hold_n
				db doconst, 100, 0
				
env_bits_n:		db 81, "ADDRESS-UNIT-BITS"
				db env_pad_n
				db doconst, 16, 0
				
env_floored_n:	db 71, "FLOORED"
				db env_bits_n
				db call, true, exit
				
env_maxchar_n:	db 72, "MAX-CHAR"
				db env_floored_n
				db doconst, 255, 0
				
env_maxd_n:		db 69, "MAX-D"
				db env_maxchar_n
				db call, dlit, 255, 255, 255, 127, exit
				
env_maxn_n:		db 69, "MAX-N"
				db env_maxd_n
				db doconst, 255, 127
				
env_maxu_n:		db 69, "MAX-U"
				db env_maxn_n
				db doconst, 255, 255
				
env_maxud_n:	db 70, "MAX-UD"
				db env_maxu_n
				db call, dlit, 255, 255, 255, 255, exit
				
env_rstack_n:	db 82, "RETURN-STACK-CELLS"
				db env_maxud_n
				db doconst, 0, 16
				
env_stack_n:	db 75, "STACK-CELLS"
				db env_rstack_n
				db doconst, 240, 15
				
environmentq_n:	db 12, "ENVIRONMENT?"
				db env_stack_n
environmentq:	db call, xfind, dup, qbranch, environmentq_1, count, lit, #namemask, and, add, twoplus, execute, true
environmentq_1:	db exit

ref environmentq

; ==============================================================================
; Optional File-Access word set

bin_n:	db 3, "BIN"
		db environmentq_n
bin:	db call, exit		; taking the recommendation and handling all files as binary

ro_n:	db 3, "R/O"
		db bin_n
ro:		db call, lit, ro_v, exit
ro_v:	db "rb", 0

wo_n:	db 3, "W/O"
		db ro_n
wo:		db call, lit, wo_v, exit
wo_v:	db "wb", 0

rw_n:	db 3, "R/W"
		db wo_n
rw:		db call, lit, rw_v, exit
rw_v:	db "r+b", 0

cstr_n:	db 68, "CSTR"	; Convert counted string to C string ( c, n, buf -->)
		db rw_n
cstr:	db call, tor, dup, rat, add, zero, swap, cpoke, rfrom, swap, cmove, exit

createfile_n:	db 11, "CREATE-FILE"
				db cstr_n
createfile:		db call, tor, twodup, filestatus, nip, qbranch, createfile_1
				db twodup, wo, openfile, drop, closefile, drop
createfile_1:	db rfrom, openfile, exit

openfile_n:	db 9, "OPEN-FILE"
			db createfile_n
openfile:	db call, rot, rot, lit, _fnamebuf, cstr
			db lit, _fnamebuf, swap, openfile_x, dup, zeroeq, exit
openfile_x:	db openfile_c
openfile_c:	call _fopen
			modstk 4
			push
			ujump next

closefile_n:	db 10, "CLOSE-FILE"
				db openfile_n
closefile:		db closefile_c
closefile_c:	call _fclose
				modstk 2
				push
				ujump next

deletefile_n:	db 11, "DELETE-FILE"
				db closefile_n
deletefile:		db call, lit, _fnamebuf, cstr, lit, _fnamebuf, deletefile_x, exit
deletefile_x:	db deletefile_c
deletefile_c:	call _remove
				modstk 2
				push
				ujump next

renamefile_n:	db 11, "RENAME-FILE"	; Note that this is the only word that uses PAD
				db deletefile_n
renamefile:		db call, lit, _fnamebuf, cstr, lit, _pad, cstr
				db lit, _pad, lit, _fnamebuf, renamefile_x, exit
renamefile_x:	db renamefile_c
renamefile_c:	call _rename
				modstk 4
				push
				ujump next

resizefile_n:	db 11, "RESIZE-FILE"
				db renamefile_n
resizefile:		db call, resizefile_x, exit
resizefile_x:	db resizefile_c
resizefile_c:	call _setsize
				modstk 6
				push
				ujump next

repositionfile_n:	db 15, "REPOSITION-FILE"
					db resizefile_n
repositionfile:		db call, rot, rot, zero, reposfile_x, exit
reposfile_x:		db reposfile_c
reposfile_c:		call _fseek
					modstk 8
					push
					ujump next

fileposition_n:	db 13, "FILE-POSITION"
				db repositionfile_n
fileposition:	db filepos_c
filepos_c:		call _ftell
				modstk 2
				push
				limm -1
				swap
				push
				testeq
				neg
				push
				ujump next

filesize_n:	db 9, "FILE-SIZE"
			db fileposition_n
filesize:	db filesize_c
filesize_c:	call _getsize
			modstk 2
			push
			limm -1
			swap
			push
			testeq
			neg
			push
			ujump next

filestatus_n:	db 11, "FILE-STATUS"
				db filesize_n
filestatus:		db call, lit, _fnamebuf, cstr, lit, _fnamebuf, filestatus_x, exit
filestatus_x:	db filestatus_c
filestatus_c:	call _access
				modstk 2
				swap
				push
				swap
				push
				ujump next

xreadchar:		db xreadchar_c
xreadchar_c:	call _getc
				pop
				push
				ujump next
				
xreadtchar:		db xreadtchar_c
xreadtchar_c:	call _getc
				swap
				limm 13
				testeq
				fjump xreadtchar_2
				call _getc
xreadtchar_1:	pop
				push
				ujump next
xreadtchar_2:	swap
				ujump xreadtchar_1

readline_n:	db 9, "READ-LINE"
			db filestatus_n
readline:	db call, tor, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readline_1:	db twodup, swap, uless, qbranch, readline_4				; buffer full?
			db rat, xreadtchar, dup, zerolt, invert, qbranch, readline_2	; end of file?
			db dup, lit, 10, 0, notequal, qbranch, readline_3		; end of line
			db over, cpoke, oneplus, branch, readline_1 
readline_2:	db drop, swap, drop, swap, sub, dup, zeroeq, qbranch, readline_5, false, branch, readline_6
readline_3:	db drop
readline_4:	db swap, drop, swap, sub
readline_5:	db true
readline_6:	db zero, rdrop, exit

readfile_n:	db 9, "READ-FILE"
			db readline_n
readfile:	db call, tor, swap, dup, rot, add, over					; c-addr, c-addr-limit, current
readfile_1:	db twodup, swap, uless, qbranch, readfile_3				; buffer full?
			db rat, xreadchar, dup, zerolt, invert, qbranch, readfile_2		; end of file?
			db over, cpoke, oneplus, branch, readfile_1 
readfile_2:	db drop
readfile_3:	db swap, drop, swap, sub, zero, rdrop, exit

xputchar:	db xputchar_c
xputchar_c:	call _putc
			modstk 4
			ujump next

writeline_n:	db 10, "WRITE-LINE"
				db readfile_n
writeline:		db call, dup, tor, writefile, lit, 13, 0, rat, xputchar, lit, 10, 0, rfrom, xputchar, exit

writefile_n:	db 10, "WRITE-FILE"
				db writeline_n
writefile:		db call, tor,
writefile_1:	db dup, zerone, qbranch, writefile_2, swap, dup, cpeek, rat, xputchar, oneplus, swap, oneminus, branch, writefile_1
writefile_2:	db twodrop, rdrop, zero, exit

flushfile_n:	db 10, "FLUSH-FILE"
				db writefile_n
flushfile:		db flushfile_c
flushfile_c:	call _fflush
				modstk 2
				push
				ujump next

includefile_n:	db 12, "INCLUDE-FILE"
				db flushfile_n
includefile:	db call, lit, _ibufcount, peek, lit, 7, 0, greater, qbranch, includefile_1, xabortq, 17, "Too many includes"
includefile_1:	db lit, _source, peek
				db twominus, zero, over, poke	; two more entries to keep fileposition before the last refill
				db twominus, zero, over, poke
				db twominus, zero, over, poke
				db twominus, lit, _ibuf, lit, _ibufcount, peek, lit, 100, 0, mult, add, over, poke
				db twominus, zero, over, poke
				db twominus, swap, over, poke
				db twominus, lit, 6, 0, over, poke
				db lit, _source, poke
				db lit, _ibufcount, dup, peek, oneplus, swap, poke
				db state, qbranch, includefile_2, interpret
includefile_2:	db exit

include_n:	db 7, "INCLUDE"
			db includefile_n
include:	db call, parsename, included, exit

included_n:	db 8, "INCLUDED"
			db include_n
included:	db call, twodup, filestatus, nip, qbranch, included_1, twodrop, exit
included_1:	db twodup, xcreate, ro, openfile, qbranch, included_2, drop, exit
included_2:	db includefile, exit

require_n:	db 7, "REQUIRE"
			db included_n
require:	db call, parsename, required, exit

required_n:	db 8, "REQUIRED"
			db require_n
required:	db call, twodup, xfind, zeroeq, qbranch, required_1, included, exit
required_1:	db twodrop, exit

; ==============================================================================
; Small subset from the optional Facility word set

beginstructure_n:	db 15, "BEGIN-STRUCTURE"
					db required_n
					db call, create, here, zero, zero, comma, xcode, 11, does, peek, exit
					
endstructure_n:	db 13, "END-STRUCTURE"
				db beginstructure_n
				db call, swap, poke, exit
				
addfield_n:	db 6, "+FIELD"
			db endstructure_n
addfield:	db call, create, over, comma, add, xcode, 11, does, peek, add, exit
			
field_n:	db 6, "FIELD:"
			db addfield_n
			db call, two, addfield, exit

cfield_n:	db 7, "CFIELD:"
			db field_n
			db call, one, addfield, exit

; ==============================================================================
; The main system loop. Intentionally placing it last so the listing of words will start with it

forth_system_n:	db 76, "FORTH-SYSTEM"
				db cfield_n
forth_system:	db call
forth_system_c:	db lit, banner_text, count, type, cr
				db decimal, false, state, poke, xsst
				db lit, autorun, count, included, branch, forth_system_1
forth_system_r:	db decimal, false, state, poke, xsst
forth_system_1:	db interpret, branch, forth_system_1
banner_text:	db 17, "Forth Model T 1.0"
autorun:		db 11, "autorun.fth"

ref forth_system ; never directly used

; ==============================================================================

end_of_image: end

; Silence known benign warnings
ref #ramtop
ref abortq
ref actionof
ref align
ref aligned
ref backslash
ref bcharb
ref bcompile
ref bin
ref brace
ref bufferc
ref bye
ref case
ref char
ref chars
ref colon
ref colonnoname
ref compilecomma
ref constant
ref createfile
ref defer
ref deletefile
ref div
ref divmod
ref do
ref doesx
ref dotbrace
ref dotquote
ref dotr
ref endcase
ref endof
ref erase
ref evaluate
ref extended
ref fileposition
ref filesize
ref filestatus
ref flushfile
ref fmmod
ref forget
ref holds
ref i
ref immediate
ref include
ref is
ref j
ref key
ref leave
ref literal
ref loop
ref lshift
ref marker
ref max
ref min
ref mod
ref move
ref multdiv
ref nonstandard
ref of
ref ploop
ref postpone
ref qdo
ref qdup
ref readfile
ref recurse
ref renamefile
ref repositionfile
ref require
ref resizefile
ref roll
ref rshift
ref rw
ref saveinput
ref savesystem
ref semicolon
ref squote
ref ssquote
ref strict
ref to
ref tuck
ref twodiv
ref twoover
ref twopeek
ref twopoke
ref udot
ref udotr
ref ugreater
ref unloop
ref unused
ref value
ref variable
ref within
ref writeline
ref xinits
