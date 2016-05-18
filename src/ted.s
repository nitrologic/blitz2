
ngerms	equ	6
cripple	equ	0	;to make a crippled version
begin	;
	;OK, lets get to it.....
	;
	;BLITZ BASIC II is underway by popular demand
	;
	;Not sure if intuition compatibility is such a good
	;thing, but let's at least get the editor underway!
	;
	include	e:myoffs
	;
doslen	equ	2048	;len of i/o buffer
	rsreset
nextitem	rs.l	1	;pointer to next line
lastitem	rs.l	1	;pointer to previous line
numchars	rs.b	1	;characters in line 
chars	rs.b	0	;actual characters.
lenitem	rs.b	0	;total length of item block
	;
	bsr	edstart
	;
outside	move	#-1,returncode
	move.l	a1,external
	beq	.skip0
	lea	tokens(pc),a0
	moveq	#13,d0
.loop	move	(a1)+,(a0)+
	dbf	d0,.loop
	bsr	dosopen
	beq	wb_exit
.skip0	opengr	
	beq	fail2
	openint	
	beq	fail3
	move.l	d0,extint
	;
	bsr	rawed+32
	;
	move.l	#doslen,a0
	add	chcols(pc),a0
	add	chcols(pc),a0
	add	chcols(pc),a0
	addq	#6,a0
	move.l	a0,d0
	moveq	#1,d1
	move.l	4,a6
	jsr	allocmem(a6)
	move.l	d0,dosbuff
	beq	fail5
	move.l	d0,a0
	add	#doslen,a0
	move.l	a0,charbuff
	add	chcols(pc),a0
	addq	#2,a0
	move.l	a0,charbuff2
	add	chcols(pc),a0
	addq	#2,a0
	move.l	a0,charbuff3
	add	chcols(pc),a0
	addq	#2,a0
	move.l	a0,charbuff4
	;
	bsr	loaddefs
	clr	defalt
	bsr	screenopen
	bmi	openfailed
	;
	bsr	donew
	beq	allocfailed
	;
	bsr	printmem
	bsr	mouseinit
	bsr	pointeron
	jsr	flink
	move.l	external(pc),d0
	beq	.edmain
	bsr	getstuff
	lea	.tomain(pc),a0
	lea	comdata(pc),a1
	move.l	#defaultreqga9+13,84(a1)
	moveq	#0,d0
	rts
	;
.edmain	lea	progfile(pc),a0
.tomain	bsr	a0toname
	move.b	filename(pc),d0
	beq	.domain
	lea	filepath(pc),a0
	move.l	charbuff(pc),a1
	move.l	a1,d7
.floop	move.b	(a0)+,(a1)+
	bne	.floop
	subq	#1,a1
	cmp.b	#':',-2(a0)	
	beq	.fskip
	move.b	#'/',(a1)+
.fskip	lea	filename(pc),a0
.floop2	move.b	(a0)+,(a1)+
	bne	.floop2
	;
	bsr	fileopen2
	bsr	showvprop
	bsr	showmice
	bsr	printmem
	;
.domain	bsr	mainedit
	bsr	mousefinit
	;
	clr	returncode
	move	defalt(pc),d0
	beq	allocfailed
	bsr	savedefs
	;
allocfailed	bsr	screenclose
	;
openfailed	;
	move.l	dosbuff(pc),a1
	move.l	#doslen,a0
	add	chcols(pc),a0
	add	chcols(pc),a0
	add	chcols(pc),a0	
	addq	#6,a0
	move.l	a0,d0
	move.l	4,a6
	jsr	freemem(a6)
fail5	bsr	rawed+32+8
	closeint
fail3	closegr
fail2	closedos
fail1	move	returncode(pc),d0
	ext.l	d0
	bra	wb_exit

edstart	include	e:wbheader
	sub.l	a1,a1
	rts

;-----------main editor loop----------------------;

mainedit	clr	mousealt
	clr	memalt
	bsr	getline
	move.l	thisitem(pc),a4
	move	anyalt(pc),d0
	beq	.skip
	move	d0,allalt
	bsr	charstobuff
	;
	bsr	makeline
	move	tokenise(pc),d0
	beq	.skip
	bsr	showthisitem
	;
.skip	lea	jumptable(pc),a0
.loop	move.b	(a0)+,d0
	beq	mainedit
	cmp.b	d0,d7
	beq	.found
	addq	#5,a0
	bra	.loop
.found	move.b	(a0)+,-(a7)
	move.l	(a0)+,a0
	move.l	a7,memstack
	jsr	(a0)
memcont	move	memalt(pc),d0
	beq	.skip
	bsr	printmem
.skip	move	mousealt(pc),d0
	beq	.skip2
	bsr	showmice
.skip2	move.b	(a7)+,d0
	tst.b	d0
	beq	mainedit
	bsr	showvprop
	bra	mainedit
	;
	rts

;-----------editor functions from jump table------;
funcs

mouseinit	move.l	#finish-pointer,d0
	moveq	#2,d1	;get chip mem!
	move.l	4.w,a6
	jsr	allocmem(a6)
	move.l	d0,a0
	move.l	d0,pointerat
	add.l	#dumsleep-pointer,d0
	move.l	d0,dumat
	;
	lea	pointer(pc),a1
	move	#(finish-pointer)/4-1,d0
.loop	move.l	(a1)+,(a0)+
	dbf	d0,.loop
	clr.l	sleepcurr
	rts

mousefinit	move.l	mywindow(pc),a0
	move.l	int(pc),a6
	jsr	clearpointer(a6)
	;
	move.l	pointerat,a1
	move.l	#finish-pointer,d0
	move.l	4.w,a6
	jmp	freemem(a6)

pointeron	movem.l	a0-a1/d0-d3/a6,-(a7)
	move.l	sleepcurr(pc),d0
	beq	.skip
	lea	myint(pc),a1
	moveq	#5,d0
	move.l	4.w,a6
	jsr	remintserver(a6)
	clr.l	sleepcurr
.skip	move.l	pointerat(pc),a1
	moveq	#(pointerf-pointer)/4-2,d0
	bsr	setpoint
	movem.l	(a7)+,a0-a1/d0-d3/a6
	rts

sleepon	movem.l	a0-a1/d0-d3/a6,-(a7)
	move.l	sleepcurr(pc),d0
	bne.s	.done
	lea	sleep(pc),a0
	bsr	makedum
	move.l	dumat(pc),a1
	moveq	#(sleepf-sleep)/8/4-2,d0
	move.l	#sleep,sleepcurr
	move	#sleepwait,sleepdelay
	bsr	setpoint
	lea	myint(pc),a1
	moveq	#5,d0
	move.l	4.w,a6
	jsr	addintserver(a6)
	movem.l	(a7)+,a0-a1/d0-d3/a6
	rts
.done	movem.l	(a7)+,a0-a1/d0-d3/a6
	bra	makemenus

setpoint	move.l	int(pc),a6
	move.l	req_window(pc),d1
	beq.s	.skip
	move.l	d1,a0
	moveq	#16,d1
	moveq	#-1,d2
	moveq	#0,d3
	movem.l	d0/a1,-(a7)
	jsr	setpointer(a6)
	movem.l	(a7)+,d0/a1
.skip	move.l	mywindow(pc),a0
	moveq	#16,d1
	moveq	#-1,d2
	moveq	#0,d3
	jsr	setpointer(a6)
	;
makemenus	move.l	req_window(pc),d0
	bne.s	menusoff
	move.l	sleepcurr(pc),d0
	bne.s	menusoff
	;
menuson	move.l	mywindow(pc),a0
	bclr	#0,25(a0)
	move.l	req_window(pc),d0
	beq.s	.skip
	move.l	d0,a0
	bclr	#0,25(a0)
.skip	rts

menusoff	move.l	mywindow(pc),a0
	bset	#0,25(a0)
	move.l	req_window(pc),d0
	beq.s	.skip
	move.l	d0,a0
	bset	#0,25(a0)
.skip	rts

sleepwait	equ	1

makedum	;a0=sleep to make
	;
	move.l	dumat(pc),a1
	addq	#4,a0
	addq	#4,a1
	moveq	#(sleepf-sleep)/8/4-3,d0
.loop	move.l	(a0)+,(a1)+
	dbf	d0,.loop
	rts

myroutine	subq	#1,sleepdelay
	bpl	.done
	;
	move	#sleepwait,sleepdelay
	move.l	sleepcurr(pc),a0
	lea	(sleepf-sleep)/8(a0),a0
	cmp.l	#sleepf,a0
	bcs	.skip
	lea	sleep(pc),a0
.skip	move.l	a0,sleepcurr
	bsr	makedum
	;
.done	moveq	#0,d0
	rts

myint	dc.l	0,0
	dc.b	2,0
	dc.l	0,0
	dc.l	myroutine

pointerat	dc.l	0
dumat	dc.l	0
sleepcurr	dc.l	0
sleepdelay	dc	0

sleep	incbin	sleep.bin
sleepf

pointer	incbin	pointer.bin
pointerf

dumsleep	ds.b	(sleepf-sleep)/8	;for altering watch!
dumsleepf

finish

blocktab	cmp	#-1,bboty
	beq	beepem
	bsr	bloksetup2
	;
	;a5=btopy, d6=btop line
	;a4=thisy, d7=this line
	;
	move.l	a5,a4
.loop2	move.l	charbuff2(pc),a1
	move	tabstop(pc),d0
	beq	.skip
	subq	#1,d0
.loop	move.b	#32,(a1)+
	dbf	d0,.loop
.skip	lea	chars(a4),a0
	move	tokenise(pc),d0
	bne	.dode
.loop4	move.b	(a0)+,(a1)+
	bne	.loop4
	bra	.loop3
.dode	bsr	detok
.loop3	cmp.l	charbuff3(pc),a1
	bcc	.skip2
	move.b	#32,(a1)+
	bra	.loop3
.skip2	move.l	a4,thisitem
	bsr	makefirst
	move.l	nextitem(a4),a4
	addq	#1,d6
	cmp	bboty(pc),d6
	bls	.loop2
	bsr	calcitem
	bra	showall

blockuntab	cmp	#-1,bboty
	beq	beepem
	bsr	bloksetup2
	;
	;a5=btopy, d6=btop line
	;a4=thisy, d7=this line
	;
	move.l	a5,a4
.loop2	move.l	charbuff2(pc),a1
	lea	chars(a4),a0
	move	tokenise(pc),d0
	bne	.dode
.loop4	move.b	(a0)+,(a1)+
	bne	.loop4
	bra	.skip3
.dode	bsr	detok
.skip3	move.l	charbuff2(pc),a1
	move.l	a1,a0
	move	tabstop(pc),d0
	beq	.loop3
	subq	#1,d0
.loop6	tst.b	(a0)+
	beq	.loop3
	dbf	d0,.loop6
.loop5	move.b	(a0)+,(a1)+
	bne	.loop5
	subq	#1,a1
.loop3	cmp.l	charbuff3(pc),a1
	bcc	.skip2
	move.b	#32,(a1)+
	bra	.loop3
.skip2	move.l	a4,thisitem
	bsr	makefirst
	move.l	nextitem(a4),a4
	addq	#1,d6
	cmp	bboty(pc),d6
	bls	.loop2
	bsr	calcitem
	bra	showall

defaults	move.l	charbuff(pc),a0
	lea	defstart(pc),a1
	moveq	#(defend-defstart)/2-1,d0
.loop0	move	(a1)+,(a0)+
	dbf	d0,.loop0
	or	#$100,defaultreqga8+12
	move.l	tokens(pc),d0
	beq	.disa
	and	#$ffff-$100,defaultreqga8+12
.disa	bsr	screenclose
	not	copok
	bsr	screenopen2
	not	copok
	lea	defaultreq(pc),a0
	bsr	openreq
	bsr	swapbords
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#10,d6
	bcs	.skip
	cmp	#26,d6
	bcc	.skip
	sub	#10,d6
	move	d6,d0
	lsr	#1,d0
	and	#6,d0	;col hit (0,2,4)
	lea	normcol(pc),a0
	lea	0(a0,d0),a1
	move	(a1),d1
	lsl	#1,d0
	add	d1,d0	;old gad
	cmp	d0,d6
	beq	.skip2
	move.l	a1,-(a7)
	move	d0,-(a7)
	bsr	dispgadd0
	move	(a7)+,d0
	bsr	swapd0bord
	move	d6,d0
	bsr	swapd0bord
	move.l	(a7)+,a0
	move	d6,d0
	and	#3,d0
	move	d0,(a0)
	cmp	#12,d6
	bcs	.loop
	bsr	setprops
	bsr	dispprops
	bra	.loop
.skip2	bsr	swapd0bord
	move	d6,d0
	bsr	dispgadd0
	move	d6,d0
	bsr	swapd0bord
	bra	.loop
.skip	cmp	#4,d6
	bcc	.end
	moveq	#0,d4
	move	propcol(pc),d4
	lea	palit(pc),a5
	move	d4,d6
	lsl	#1,d6
	move	d6,d5
	lsl	#1,d6
	add	d5,d6
	add	d6,a5	;palit offset
.ploop	bsr	propadj
	bsr	getinput2
	tst.l	d7
	beq	.ploop
	bsr	propadj
	bra	.loop
.end	cmp	#26,d6
	bcc	.done
	moveq	#0,d0
	cmp.l	#32,int1
	bcs	.sskip1
	clr.l	int1
	move	#$3000,defaultreqla47
	moveq	#-1,d0
.sskip1	cmp.l	#10,int2
	bcs	.sskip2
	clr.l	int2
	move	#$3000,defaultreqla48
	moveq	#-1,d0
.sskip2	cmp.l	#80,int3
	bcs	.sskip
	clr.l	int3
	move	#$3000,defaultreqla49
	moveq	#-1,d0
.sskip	tst.l	d0
	beq	.sskip3
	bsr	beepem
	lea	defaultreqga4(pc),a1
	move.l	a1,a0
	moveq	#2,d0
.lll	and	#$ffff-$80,12(a1)
	lea	defaultreqga5-defaultreqga4(a1),a1
	dbf	d0,.lll
	move.l	req_window(pc),a1
	lea	defaultreq(pc),a2
	moveq	#3,d0
	callint	refreshglist
.sskip3	moveq	#5,d0
	btst	#7,defaultreqga7+13
	bne	.eskip
	moveq	#7,d0
.eskip	move	d0,fonthite2
	addq	#1,d0
	move	d0,fonthite
	mulu	bmapmul(pc),d0
	move	d0,fontmul
	move	defaultreqga8+12,d0
	and	#128,d0
	lsr	#7,d0
	move	d0,tokenise
	move	defaultreqga9+12,d0
	and	#128,d0
	lsr	#7,d0
	move	d0,makeicons
	move	int1+2(pc),hscmarg
	move	int2+2(pc),vscmarg
	move	int3+2(pc),tabstop
	bra	.loop
	;
.done	bne	.skip3
	;
	move	normcol(pc),color
	move	d6,defalt
	bsr	swapbords
	move.l	charbuff(pc),a0
	move	(a0),d0
	cmp	fonthite(pc),d0
	beq	.skip5
	;
	move	cursy(pc),d0
	move.l	thisitem(pc),a0
.loopy	cmp	edrows(pc),d0
	bcs	.skip6
	subq	#1,d0
	move.l	lastitem(a0),a0
	bra	.loopy
.skip6	move.l	a0,thisitem
	move	d0,cursy
	bsr	calcpos
	;
.skip5	bsr	screenclose
	bsr	screenopen2
	;
	move.l	charbuff(pc),a0
	move	6(a0),d0
	cmp	tokenise(pc),d0
	beq.s	.byebye
	tst	d0
	bne	detokall2
	bra	tokall2
.byebye	rts
	;
.skip3	;cancel!
	;
	move.l	charbuff(pc),a5
	bsr	setdefs
	bsr	colors
	move	normcol(pc),color
	bsr	swapbords
	bsr	screenclose
	bra	screenopen2
	
prtfile	move.l	#prtreq,a0
	bsr	openreq
	move.l	#prtreqga1,a0
	move.l	#prtreq,a2
	bsr	actgad
	;
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#2,d6
	beq	.print
	cmp	#3,d6
	beq	.cancel
	bra	.loop
	;
.print	move.l	#prtname,d1
	move.l	#1006,d2
	calldos	open
	;
	move.l	d0,d7
	beq	.cancel
	;
	move	numlines(pc),d6
	move.l	firstitem(pc),a4
	;
.ploop	move.l	charbuff(pc),a0
	lea	chars(a4),a1
	move.l	a0,d2
.dloop	move.b	(a1)+,(a0)+
	beq	.ddone
	bpl	.dloop
	move.b	-(a0),d0
	lsl	#8,d0
	move.b	(a1)+,d0
	;bold on
	move.b	#27,(a0)+
	move.b	#'[',(a0)+
	move.b	#'1',(a0)+
	move.b	#'m',(a0)+
	;
	bsr	findtoke
	;
.tloop	move.b	(a3)+,(a0)+
	bne	.tloop
	;bold off
	move.b	#27,-1(a0)
	move.b	#'[',(a0)+
	move.b	#'2',(a0)+
	move.b	#'2',(a0)+
	move.b	#'m',(a0)+
	bra	.dloop
	;
.ddone	move.b	#10,-1(a0)
	move.l	a0,d3
	sub.l	d2,d3
	move.l	d7,d1
	jsr	write(a6)
	move.l	(a4),a4
	subq	#1,d6
	bne	.ploop
	;
	move.l	d7,d1
	jsr	close(a6)
	;
.cancel	rts

xtraat	dc.l	0
xtranull	dc.l	0
xtratxt	dc.b	'.xtra',0
infotxt	dc.b	'.info',0

openinfo	lea	infotxt(pc),a1
	bsr	openmore
	moveq	#-2,d2
	jsr	lock(a6)
	move.l	d0,d1
	beq	.skip2	;not there?
	jsr	unlock(a6)
	bra	.skip
	;
.skip2	move.l	xtraat(pc),d1
	move.l	#1006,d2
	jsr	open(a6)
	move.l	d0,d7
	rts
.skip	moveq	#0,d7
	rts

openxtra	lea	xtratxt(pc),a1
	bsr	openmore
	jsr	open(a6)
	move.l	d0,d7
	rts
	
openmore	move.l	xtraat(pc),a0
	move.l	a0,d1
.loop	tst.b	(a0)+
	bne	.loop
	subq	#1,a0
	move.l	a0,xtranull
.loop2	move.b	(a1)+,(a0)+
	bne	.loop2
	rts

closextra	move.l	xtranull(pc),a0
	clr.b	(a0)
	rts

fileopen	bsr	erase
	bsr	getfname
fileopen2	move.l	#1005,d2
	move.l	d7,xtraat
	bsr	doopen
	clr	allalt
	bsr	doclean
	bsr	fillbuff
	bne	.ok
	bsr	doclose
	bra	donew
.ok	bsr	sleepon
	bsr	blokdis
	bsr	freeall
	move	#1,numlines
	bsr	readline
	bsr	allocline
	move.l	a0,firstitem
	move.l	a0,thisitem
	clr.l	lastitem(a0)
	move.l	a0,enditem
.loop	move.l	a0,a4
	bsr	readline
	bmi	.done
	bsr	allocline
	move.l	a0,enditem
	move.l	a0,nextitem(a4)
	addq	#1,numlines
	move.l	a4,lastitem(a0)
	bra	.loop
.done	bsr	doclose
	;
	move.l	loadxtra(pc),d0
	beq	.noxtra
	;
	move.l	d0,a5
	move.l	#1005,d2
	bsr	openxtra
	beq	.noxtra2
	jsr	(a5)
	move.l	d7,d1
	jsr	close(a6)
.noxtra2	bsr	closextra
.noxtra	;
	clr.l	scloc
	clr	chloc
	clr.l	cursx
	clr.l	cursoff
	clr	cursy
	bsr	showhprop
	bsr	showall
	bsr	pointeron
	;
showfname	lea	filename(pc),a0
	lea	newsctit(pc),a1
	move.l	#'File',(a1)+
	move	#': ',(a1)+
.loop2	move.b	(a0)+,(a1)+
	bne	.loop2
showme	move.l	myscreen(pc),a0
	moveq	#-1,d0
	move.l	int(pc),a6
	jmp	showtitle(a6)

bakname	dc.b	'.bak',0
	even

filesave	;
	ifeq	cripple
	;
	lea	filetext2(pc),a2
	bsr	getfname2
	move.l	d7,xtraat
	;
	;do the delete/rename bit for .bak files
	;
	move.l	dosbuff(pc),a1
	move.l	a1,d1
	move.l	d7,a0
.loop	move.b	(a0)+,(a1)+
	bne	.loop
	subq	#1,a1
	lea	bakname(pc),a0
.loop2	move.b	(a0)+,(a1)+
	bne	.loop2
	calldos	deletefile
	move.l	d7,d1
	move.l	dosbuff(pc),d2
	jsr	rename(a6)
	;
	;
realsave	move.l	#1006,d2
	bsr	doopen
	bsr	sleepon
	move.l	firstitem(pc),a2
	move	numlines(pc),d5
	subq	#1,d5
.loop	moveq	#0,d4
	move.b	numchars(a2),d4
	lea	chars(a2),a3
	move	tokenise(pc),d0
	bne	.nocr
	move.b	#10,-1(a3,d4)
.nocr	move.l	d7,d1
	move.l	a3,d2
	move.l	d4,d3
	jsr	write(a6)
	clr.b	-1(a3,d4)
	tst.l	d0
	bpl	.skip2
	lea	error3(pc),a0
	bsr	error
	bra	.skip3
.skip2	move.l	nextitem(a2),a2
	dbf	d5,.loop
	clr	allalt
.skip3	bsr	doclose
	move.l	savextra(pc),d0
	beq	.skip
	move.l	d0,a5
	move.l	#1006,d2
	bsr	openxtra
	beq	.skipex
	jsr	(a5)
	move.l	d7,d1
	jsr	close(a6)
.skipex	bsr	closextra
.skip	btst	#7,defaultreqga9+13
	beq	.noicons
	;
	;OK, we're gonna save a .info file with the source.
	;
	bsr	openinfo
	beq	.noicons2
	move.l	#srcinfo,d2
	move.l	#srcinfof-srcinfo,d3
	move.l	d7,d1
	jsr	write(a6)
	move.l	d7,d1
	jsr	close(a6)
.noicons2	bsr	closextra
	;
.noicons	bsr	pointeron
	bra	showfname
	;
	endc
	;
	rts

srcinfo	incbin	src.info
srcinfof	even

	include	aboutreq

	include	specialreq

doabout	lea	aboutreq(pc),a0
	bsr	openreq
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#2,d6
	beq	.special
	cmp	#1,d6
	bne	.loop
	rts
.special	lea	specialreq(pc),a0
	bsr	openreq
.loop2	bsr	getinput
	cmp	#-1,d7
	bne	.loop2
	cmp	#1,d6
	bne	.loop2
	rts

find	clr	findreqsg1+8
	clr	findreqsg1+12
	clr	findreqsg2+8
	clr	findreqsg2+12
	lea	findreqla16(pc),a0
	move.l	charbuff(pc),a1
.loop2	move.b	(a0)+,(a1)+
	bne	.loop2
	lea	findreqla17(pc),a0
	move.l	charbuff2(pc),a1
.loop3	move.b	(a0)+,(a1)+
	bne	.loop3
	lea	findreq(pc),a0
	bsr	openreq
.loopzz	lea	findreqga1(pc),a0
	lea	findreq(pc),a2
	bsr	actgad
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#2,d6
	beq	next	;.loop
	cmp	#1,d6
	beq	.actrep
	cmp	#3,d6
	beq	next
	cmp	#4,d6
	beq	previous
	cmp	#5,d6
	beq	replace
	cmp	#6,d6
	beq	replaceall
	cmp	#8,d6
	beq	.loopzz
	move.l	charbuff(pc),a0
	lea	findreqla16(pc),a1
.loop4	move.b	(a0)+,(a1)+
	bne	.loop4
	move.l	charbuff2(pc),a0
	lea	findreqla17(pc),a1
.loop5	move.b	(a0)+,(a1)+
	bne	.loop5
	rts
.actrep	;activate replace gadget...
	lea	findreqga2(pc),a0
	lea	findreq(pc),a2
	bsr	actgad
	bra	.loop
	;
replaceall	move.b	findreqla16(pc),d0
	beq	beepem
	lea	repall(pc),a0
	bsr	openreq
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#3,d6
	beq	.skip
	bsr	sleepon	;***
	move.l	a4,d0
	cmp	#1,d6
	beq	.here
	move.l	firstitem(pc),d0
.here	move.l	d0,a4
	move.l	a4,thisitem
	bsr	replace2
	move.l	nextitem(a4),d0
	bne	.here
	bsr	calcitem
	bsr	showall
	bra	pointeron	;***
.skip	rts

next	move.l	nextitem(a4),d0
	beq	beepem
	move.l	d0,a4
	moveq	#1,d6
	;	
dosearch	;direction in d6 (1 or -1), line in a4
	move.b	findreqla16(pc),d0
	beq	beepem
	bsr	sleepon
	move	curstop(pc),d7
	add	cursy(pc),d7
	add	d6,d7
.loop	bsr	isithere
	bpl	.gotit
	add	d6,d7
	bmi	beepem
	cmp	numlines(pc),d7
	bcc	beepem
	tst	d6
	bpl	.skip
	move.l	lastitem(a4),a4
	bra	.loop
.skip	move.l	nextitem(a4),a4
	bra	.loop
.gotit	bsr	pointeron
	move	d0,-(a7)
	move	d7,d0
	move.l	thisitem(pc),a4
	bsr	gotolined0
	move	(a7)+,d1
	bsr	horizpos
	bra	showhprop

previous	move.l	lastitem(a4),d0
	beq	beepem
	move.l	d0,a4
	moveq	#-1,d6
	bra	dosearch

replace	move.b	findreqla16(pc),d0
	beq	beepem
 	bsr	replace2
	bne	beepem
	bra	showall

replace2	;do all replaces on a4 line
	;return eq if at least one was done!
	;
	bsr	isithere
	bmi	.skip
	move	#-1,allalt
.big	move.l	charbuff2(pc),a0
	moveq	#0,d1
.loop	cmp	d0,d1
	bcc	.skip2
	move.b	(a1)+,(a0)+
	addq	#1,d1
	bra	.loop
.skip2	lea	findreqla16(pc),a2
.loopb	tst.b	(a2)+
	beq	.skip2a
	addq	#1,a1
	;addq	#1,d1
	bra	.loopb
.skip2a	;move	d1,-(a7)
	lea	findreqla17(pc),a2
.loop2	addq	#1,d1
	move.b	(a2)+,(a0)+
	bne	.loop2
	subq	#1,a0
	subq	#1,d1
	move	d1,-(a7)
.loop3	move.b	(a1)+,(a0)+
	bne	.loop3
	subq	#1,a0
.loop4	cmp.l	charbuff3(pc),a0
	bcc	.skip3
	move.b	#32,(a0)+
	bra	.loop4
.skip3	move.l	charbuff2(pc),a2
	bsr	charstobuff
	bsr	makeline
	move	(a7)+,d5
	bsr	isithere2
	bpl	.big
	moveq	#0,d0
	rts
.skip	moveq	#-1,d0
	rts

escape	;
	bsr	erase
	move.l	menu2(pc),d0
	beq	.skip
	;
	;free up external menus
	;
	move.l	d0,a1
	move.l	18(a1),a2
	moveq	#menu2name-menu2,d0
	move.l	4,a6
	jsr	freemem(a6)
.loop	move.l	a2,a1
	moveq	#m2i1text-menu2item1,d0
	move.l	(a1),a2
	jsr	freemem(a6)
	cmp	#0,a2
	bne	.loop
	;
.skip	bsr	doclean
	bsr	freemice
	clr	allalt
	lea	12(a7),a7
	bra	freeall

cursdown	move	qualifier(pc),d0
	and	#3,d0
	beq	.skip
	move	curstop(pc),d0
	add	cursy(pc),d0
	add	edrows(pc),d0
	cmp	numlines(pc),d0
	bcs	gotolined02
	move	numlines(pc),d0
	subq	#1,d0
	bra	gotolined02
.skip	move.l	nextitem(a4),d0
	bne	cursdownok
beepem	bsr	pointeron
	move.l	myscreen(pc),a0
	move.l	int(pc),a6
	jmp	displaybeep(a6)
cursdownok	;
	move.l	d0,thisitem
	;
movecdown	move	curstop(pc),d0
	add	edrows(pc),d0
	cmp	numlines(pc),d0
	bcc	godown
	move	cursy(pc),d0
	addq	#1,d0
	add	vscmarg(pc),d0
	cmp	edrows(pc),d0
	bcs	godown
	add	#1,curstop
	bsr	scrupall
	bra	showbotitem

cursup	move	qualifier(pc),d0
	and	#3,d0
	beq	.skip
	move	curstop(pc),d0
	add	cursy(pc),d0
	cmp	edrows(pc),d0
	bcc	.skip2
	moveq	#0,d0
	bra	gotolined02
.skip2	sub	edrows(pc),d0
	bra	gotolined02
.skip	move.l	lastitem(a4),d0
	beq	beepem
	;
	move.l	d0,thisitem
	move	curstop(pc),d0
	beq	goup
	move	cursy(pc),d0
	cmp	vscmarg(pc),d0
	bhi	goup
	sub	#1,curstop
	bsr	scrdownall
	bra	showtopitem

markindent	;calulate indented block
	lea	chars(a4),a0
	tst.b	(a0)
	beq	beepem
	moveq	#-1,d0
.loop	addq	#1,d0
	cmp.b	#32,(a0)+
	beq	.loop
.find	;find above bits
	move.l	a4,a1
	move	curstop(pc),d1
	add	cursy(pc),d1
	move	d1,d2
.uploop	beq	.dodown
	move.l	lastitem(a1),a1
	lea	chars(a1),a0
	moveq	#-1,d3
.loop2	addq	#1,d3
	cmp.b	#32,(a0)+
	beq	.loop2
	tst.b	-(a0)
	beq	.upline
	cmp	d0,d3
	blt	.dodown
.upline	subq	#1,d1
	bra	.uploop
	;
.dodown	;find down bits
	move.l	a4,a1
.doloop	move.l	nextitem(a1),d3
	beq	.downdone
	move.l	d3,a1
	lea	chars(a1),a0
	moveq	#-1,d3
.loop3	addq	#1,d3
	cmp.b	#32,(a0)+
	beq	.loop3
	tst.b	-(a0)
	beq	.downline
	cmp	d0,d3
	blt	.downdone
.downline	addq	#1,d2
	bra	.doloop
	;
.downdone	;d1=top of block, d2=bottom
	;
	move	d1,btopy
	clr	btopx
	move	d2,bboty
	move	chcols(pc),d0
	subq	#1,d0
	move	d0,bbotx
	bsr	bloken
	bra	showall

insertline	bsr	allocnull
	beq	outmem
	move	#-1,allalt
	move	curstop(pc),d0
	add	cursy(pc),d0
	bsr	blokadj
	add	#1,numlines
	move.l	lastitem(a4),d1
	beq	.skip
	move.l	d1,a1
	move.l	a0,nextitem(a1)
	move.l	a1,lastitem(a0)
	bra	.skip2
.skip	move.l	a0,firstitem
.skip2	move.l	a4,nextitem(a0)
	move.l	a0,lastitem(a4)
	move.l	a0,thisitem
	move	cursy(pc),d7
	bra	scrolldown

deleteline	move	#-1,allalt
	cmp	#-1,bboty
	beq	.skip0
	;
	;adjust block
	move	curstop(pc),d0
	add	cursy(pc),d0
	cmp	btopy(pc),d0
	bcs	.skip0z
	cmp	bboty(pc),d0
	bhi	.skip0
	sub	#1,bboty
	move	bboty(pc),d0
	cmp	btopy(pc),d0
	bcc	.skip0
	clr	bboty
.skip0z	sub	#1,btopy
	sub	#1,bboty
	;
.skip0	move.l	nextitem(a4),d7
	beq	freelast
	sub	#1,numlines
	move.l	lastitem(a4),d6
	bsr	freethis
	move.l	d7,a1
	move.l	d6,lastitem(a1)
	bne	.skip
	move.l	a1,firstitem
	bra	.skip2
.skip	move.l	d6,a0
	move.l	d7,nextitem(a0)
.skip2	move.l	d7,thisitem
	move	cursy(pc),d7
	bsr	scrollup
	bra	showbotitem
freelast	move.l	lastitem(a4),d6
	beq	freeonly
	sub	#1,numlines
	bsr	freethis
	move.l	d6,a0
	move.l	a0,thisitem
	move.l	a0,enditem
	clr.l	nextitem(a0)
	move	curstop(pc),d0
	beq	.skip
	sub	#1,curstop
	move	cursy(pc),d7
	bsr	scrollup
	bsr	scrdownall
	bra	showtopitem
.skip	move	cursy(pc),d7
	bsr	scrollup
	bra	goup
freeonly	bsr	freethis
	bra	allocfirst

join	move.l	nextitem(a4),d0
	beq	beepem
	move	#-1,allalt
	move.l	d0,a5
	move.l	chmap(pc),a2
	add	chloc(pc),a2
	move	chcols(pc),d0
.loop	subq	#1,d0
	bmi	.skip
	cmp.b	#32,0(a2,d0)
	beq	.loop
.skip	addq	#1,d0	;offset for join!
	;
	cmp	#-1,bboty
	beq	.sskip
	move	curstop(pc),d1
	add	cursy(pc),d1
	cmp	bboty(pc),d1
	bcc	.sskip
	addq	#1,d1
	cmp	btopy(pc),d1
	beq	.nexttop
	bcs	.sskip2
	cmp	bboty(pc),d1
	beq	.nextbot
	bra	.sskip3
.nexttop	cmp	bboty(pc),d1
	beq	.nextboth
	add	d0,btopx
	bra	.sskip4
.nextbot	cmp	btopy(pc),d1
	beq	.nextboth
	add	d0,bbotx
	bsr	blokfix
	bra	.sskip3
.nextboth	move	bbotx(pc),d3
	sub	btopx(pc),d3
	move	btopx(pc),d2
	add	d0,d2
	move	d2,btopx
	add	d3,d2
	move	d2,bbotx
.sskip4	bsr	blokfix
.sskip2	sub	#1,btopy
.sskip3	sub	#1,bboty
.sskip	move.l	charbuff2(pc),a0
	lea	0(a0,d0),a1
.loop2	subq	#1,d0
	bmi	.skip2
	move.b	0(a2,d0),0(a0,d0)
	bra	.loop2
.skip2	lea	chars(a5),a0
	move	tokenise(pc),d0
	bne	.skip2a
.loop3	move.b	(a0)+,(a1)+
	bne	.loop3
	subq	#1,a1
	bra	.skip3
.skip2a	bsr	detok
.skip3	cmp.l	charbuff3(pc),a1
	bcc	.skip4
	move.b	#32,(a1)+
	bra	.skip3
.skip4	move.l	charbuff2(pc),a2
	bsr	charstobuff
	bsr	makeline
	move.l	nextitem(a4),a1
	;
	move.l	nextitem(a1),d0
	beq	.end
	move.l	d0,a0
	move.l	a0,nextitem(a4)
	move.l	a4,lastitem(a0)
	bra	.done
.end	move.l	a4,enditem
	clr.l	nextitem(a4)
.done	moveq	#0,d0
	add.b	numchars(a1),d0	;was move.b
	move.l	4,a6
	move	d0,memalt
	jsr	freemem(a6)
	sub	#1,numlines
	bra	showall

split	move	#-1,allalt
	move	cursoff(pc),d0
	add	cursx(pc),d0
	cmp	#-1,bboty
	beq	.sskip
	move	curstop(pc),d2
	add	cursy(pc),d2
	cmp	bboty(pc),d2
	bhi	.sskip
	beq	.sonbot
	cmp	btopy(pc),d2
	bcs	.sskip2
	bne	.sinmid
	;
.sontop	cmp	bboty(pc),d2
	beq	.sonboth
	cmp	btopx(pc),d0
	bhi	.sinmid
	move	btopx(pc),d2
	sub	d0,d2
	move	d2,btopx
	bra	.sskip2
	;
.sonbot	cmp	btopy(pc),d2
	beq	.sonboth
	cmp	bbotx(pc),d0
	bhi	.sskip
.sskip3	move	bbotx(pc),d2
	sub	d0,d2
	move	d2,bbotx
	bra	.sinmid
	;
.sonboth	cmp	bbotx(pc),d0
	bhi	.sskip
	cmp	btopx(pc),d0
	bhi	.sskip3
	move	bbotx(pc),d3
	sub	btopx(pc),d3
	move	btopx(pc),d2
	sub	d0,d2
	move	d2,btopx
	add	d3,d2
	move	d2,bbotx
	bsr	blokfix
	;
.sskip2	add	#1,btopy
.sinmid	add	#1,bboty
.sskip	move.l	charbuff2(pc),a0
	move.l	chmap(pc),a2
	add	chloc(pc),a2
.loop	move.b	0(a2,d0),(a0)+
	move.b	#32,0(a2,d0)
	addq	#1,d0
	cmp	chcols(pc),d0
	bcs	.loop
.loop3	cmp.l	charbuff3(pc),a0
	bcc	.skipit
	move.b	#32,(a0)+
	bra	.loop3
.skipit	bsr	charstobuff
	bsr	makeline
	move.l	charbuff2(pc),a2
	bsr	charstobuff
	;
	move.l	d5,d0
	add	#lenitem,d0
	moveq	#1,d1
	move.l	4,a6
	bsr	alloc2
	move.l	d0,a0
	move.b	d5,numchars(a0)
	lea	chars(a0),a0
	move.l	charbuff(pc),a1
.loop2	move.b	(a1)+,(a0)+
	bne	.loop2
	move.l	d0,a0
	move.l	a4,lastitem(a0)
	move.l	nextitem(a4),d1
	beq	.end
	move.l	d1,a1
	move.l	a0,lastitem(a1)
	move.l	a1,nextitem(a0)
	bra	.skip
.end	move.l	a0,enditem
	clr.l	nextitem(a0)
.skip	move.l	a0,nextitem(a4)
	add	#1,numlines
	bra	showall

return	move	qualifier(pc),d0
	and	#3,d0
	bne	split
	bsr	allocnull
	beq	outmem
	move	#-1,allalt
	move	curstop(pc),d0
	add	cursy(pc),d0
	addq	#1,d0
	bsr	blokadj 	
	add	#1,numlines
	move.l	nextitem(a4),d1
	bne	.skip
	move.l	a0,nextitem(a4)
	move.l	a4,lastitem(a0)
	move.l	a0,enditem
	bra	.skip2
.skip	move.l	d1,a1
	move.l	a0,lastitem(a1)
	move.l	a0,nextitem(a4)
	move.l	a1,nextitem(a0)
	move.l	a4,lastitem(a0)
.skip2	move.l	a0,thisitem
	clr	cursx
	move	cursoff(pc),d0
	beq	.skip4
	clr	cursoff
	bsr	refresh
.skip4	bsr	showhprop
	move	cursy(pc),d0
	addq	#1,d0
	add	vscmarg(pc),d0
	cmp	edrows(pc),d0
	bcs	.skip3
	add	#1,curstop
	move	edrows(pc),-(a7)
	move	cursy(pc),d0
	addq	#1,d0
	move	d0,edrows
	move	pixrows(pc),-(a7)
	mulu	fontmul(pc),d0
	move	d0,pixrows
	bsr	scrupall
	move	(a7)+,pixrows
	move	(a7)+,edrows
	rts
.skip3	move	cursy(pc),d7
	addq	#1,d7
	bsr	scrolldown
	bra	movecdown

top	moveq	#0,d0
	bra	gotolined0

bottom	move	numlines(pc),d0
	subq	#1,d0
	bra	gotolined0

gadget	;some one hit my vprop gadget! - what a bastard!
	;
	move	edrows(pc),d0
	cmp	numlines(pc),d0
	bcs	.skip
	rts
.skip	move	curstop(pc),d0
	add	cursy(pc),d0
	move	d0,bloktopx
	bsr	cursor
.loop	move	prop1sinfo+4(pc),d0
	mulu	numlines(pc),d0
	swap	d0
	cmp	bloktopx(pc),d0
	beq	.skip2
	move	d0,bloktopx
	bsr	cursor
	move	bloktopx(pc),d0
	bsr	gotolined02
	bsr	cursor
	bsr	showrow
.skip2	bsr	getinput2
	tst.l	d7
	beq	.loop
	bsr	cursor
	move	prop1sinfo+4(pc),d0
	mulu	numlines(pc),d0
	swap	d0
	bra	gotolined02
	;
gotolined0	;move to line pointed at by d0
	move	#-1,-(a7)	;change hprop
;	clr	cursoff
	clr	cursx
	bra	gd03
gotolined02	clr	-(a7)	;leave hprop
gd03	move	curstop(pc),d1
	move	d1,-(a7)
	add	cursy(pc),d1
	cmp	d1,d0
	beq	.skip
	bcc	.skip2
.loop	subq	#1,d1
	move.l	lastitem(a4),a4
	cmp	d1,d0
	bne	.loop
	bra	.skip
.skip2	addq	#1,d1
	move.l	nextitem(a4),a4
	cmp	d1,d0
	bne	.skip2
.skip	move.l	a4,thisitem
	move	vscmarg(pc),d1
	move	d0,d2
	sub	d1,d0
	bpl	.skip3
.skip5	clr	curstop
	move	d2,cursy
	bra	.done
.skip3	move	numlines(pc),d3
	cmp	edrows(pc),d3
	bls	.skip5
	sub	edrows(pc),d3
	add	vscmarg(pc),d3
	cmp	d3,d2 
	bcs	.skip4
	sub	vscmarg(pc),d3
	move	d3,curstop
	sub	d3,d2
	move	d2,cursy
	bra	.done
.skip4	move	d1,cursy
	move	d0,curstop
.done	bsr	calcpos
	move	(a7)+,d1
	cmp	curstop(pc),d1
	beq	.noshowall
	bsr	showall
.noshowall	tst	(a7)+
	bne	showhprop
	rts

extmenu	;an external menu item has been found!
	;
	;do the menu sub, and, at return,
	;jump to line d0, unless d0=-1
	;d0=-2 for showall
	;
	move.l	menurout(pc),d0
	beq	.skip
	move.l	d0,a0
	lsr	#4,d6
	move.l	firstitem(pc),a5
	move	numlines(pc),d7
	jsr	(a0)
	move	d0,-(a7)
	bsr	showme
	move	(a7)+,d0
	move.l	thisitem(pc),a4
	cmp	#-2,d0
	beq	showall
	cmp	#-1,d0
	bne	gotolined0
.skip	rts

menu	;menu number in d6
	;
	move.b	d6,d0
	and	#15,d0
	lsr	#1,d6
	and	#$f0,d6
	cmp.b	#4,d0
	beq	extmenu
	or	d0,d6
	;
	;convert menu number to byte (hi nyb=item, lo=menu)
	;
	lea	menutable(pc),a0
.loop	move.b	(a0)+,d0
	cmp.b	#255,d0
	beq	.done
	cmp.b	d6,d0
	beq	.found
	addq	#5,a0
	bra	.loop
.found	move.b	(a0)+,-(a7)
	move.l	(a0)+,a0
	jsr	(a0)
	move.b	(a7)+,d0
	tst.b	d0
	bne	showvprop
.done	rts

new	;
	bsr	erase
	bsr	doclean
	bsr	freeall
	lea	deftitname(pc),a0
	lea	newsctit(pc),a1
.loop	move.b	(a0)+,(a1)+
	bne	.loop
	bsr	showme
	bra	donew

deftitname	dc.b	'Blitz Basic 2 Editor V1.0',0
	even

blokkill	;kill the block
	cmp	#-1,bboty
	beq	beepem
	clr	blokop
	move	#-1,allalt
	bsr	bloksetup
	moveq	#0,d5
	move.l	charbuff2(pc),a0
.loop	cmp	btopx(pc),d5
	bcc	.skip0
	bsr	getbchar2
	move.b	d0,(a0)+
	bra	.loop
.skip0	move.l	a0,-(a7)
.skip	cmp	bboty(pc),d6
	bcc	.done0
	move.l	nextitem(a5),-(a7)
	bsr	freeupa5
	move.l	(a7)+,a5
	addq	#1,d6
	bra	.skip
.done0	cmp	btopy(pc),d6
	beq	.done
	bsr	detoka5
.done	move.l	(a7)+,a0
	move	bbotx(pc),d5
.done3	addq	#1,d5
	cmp	chcols(pc),d5
	bcc	.done2
	bsr	getbchar2
	subq	#1,d5
	move.b	d0,(a0)+
	bra	.done3
.done2	cmp.l	charbuff3(pc),a0
	bcc	.done4
	move.b	#32,(a0)+
	bra	.done2
.done4	move	btopx(pc),d0
	bne	.doit
	move	bbotx(pc),d0
	addq	#1,d0
	cmp	chcols(pc),d0
	bcs	.doit
	bsr	freeupa5
	bra	.rest
.doit	move.l	a5,a4
	move.l	a4,thisitem
	bsr	makefirst
.rest	bsr	blokdis
	move	curstop(pc),d0
	add	cursy(pc),d0
	cmp	numlines(pc),d0
	bcs	.done5
	move	numlines(pc),d0
	subq	#1,d0
	move	d0,curstop
	clr	cursy
	bsr	calcitem
	bsr	showall
	move	curstop(pc),d0
	bra	gotolined02
.done5	bsr	calcitem
	bra	showall

blocksave	move	qualifier(pc),d0
	and	#3,d0
	beq.s	bloksave
	bsr	blokkill
	bra	showvprop
	;
bloksave	;
	ifeq	cripple
	;
	cmp	#-1,bboty
	beq	beepem
	lea	bloktext2(pc),a2
	bsr	getbname
	move.l	#1006,d2
	bsr	doopen
	move.l	d7,-(a7)
	move.l	thisitem(pc),a4
	bsr	bloksetup
	move.l	(a7)+,d7
	move	#-1,blokop
.loop	bsr	getbchar
	bmi	.done
	beq	.newline
	move.b	d0,bloktopx
	move.l	d7,d1
	move.l	#bloktopx,d2
	moveq	#1,d3
	jsr	write(a6)
	bra	.loop
.newline	move.b	#10,bloktopx
	move.l	d7,d1
	move.l	#bloktopx,d2
	moveq	#1,d3
	jsr	write(a6)
	moveq	#0,d5
.loop3	cmp	bboty(pc),d6
	bcs	.bok
	move	tokenise(pc),d0
	beq	.loop
	bsr	detoka5
	moveq	#0,d5
	bra	.loop
	;
.bok	bsr	detoka5
	move.l	d7,d1
	move.l	charbuff3(pc),a0
	moveq	#0,d3
	move.b	(a0),d3
	move.b	#10,0(a0,d3)
	addq	#1,a0
	move.l	a0,d2
	move.l	d7,d1
	jsr	write(a6)
	tst.l	d0
	bpl	.skip2
	lea	error3(pc),a0
	bsr	error
	bra	.done
.skip2	move.l	nextitem(a5),a5
	addq	#1,d6
	bra	.loop3	
.done	bra	doclose
.skip	;
	endc
	;
	rts

blokload	;load block from disk
	clr	blokop
	lea	bloktext(pc),a2
	bsr	getbname
	move.l	thisitem(pc),a4
	move.l	#1005,d2
	bsr	doopen
	bsr	fillbuff
	beq	doclose
	;
	move	#-1,mousealt
	move	#-1,allalt
	move.l	chmap(pc),a2
	add	chloc(pc),a2
	move.l	a2,a0
	add	chcols(pc),a0
	move.l	a0,bloktopx
	move	cursoff(pc),d1
	add	cursx(pc),d1
	move.l	charbuff(pc),a3
	moveq	#0,d4
.loop	cmp	d1,d4
	bcc	.skip
	move.b	(a2)+,d0
	bclr	#7,d0
	move.b	d0,(a3)+
	addq	#1,d4
	bra	.loop
.skip	move.l	a2,-(a7)
	bsr	readline
	beq	done2
	bsr	bloadadd
	bsr	makeline2
	;
.big	bsr	readline
	bmi	done3
	beq	done4
	bsr	allocline
	bsr	linka0
	bra	.big
done3	bsr	allocnull
	beq	outmem
	bsr	linka0
	move.l	charbuff(pc),a3
	bra	done
done2	bsr	bloadadd
	bra	done
done4	;
	bsr	allocline
	bsr	linka0
	;
	move.l	charbuff(pc),a3
	bsr	bloadadd
done	bsr	doclose
	move.l	(a7)+,a2
.loop	cmp.l	bloktopx(pc),a2
	bcc	blalldone
	cmp.l	charbuff2(pc),a3
	bcc	blalldone
	move.b	(a2)+,d0
	bclr	#7,d0
	move.b	d0,(a3)+
	bra	.loop
blalldone	move.l	charbuff(pc),a2
	bsr	charstobuff
	bsr	makeline
	bsr	calcitem
	bsr	blokdis
	bra	showall

blokcopy2	bsr	blokcopy
	bra	showvprop

blokcopy	;copy blok
	cmp	#-1,bboty
	beq	beepem
	clr	blokop
	move	#-1,allalt
	move	curstop(pc),d7
	add	cursy(pc),d7
	cmp	btopy(pc),d7
	bcs	bcopyok
	bne	.skip
	bsr	inbloks
	bne	.skip
	;
	;same as first line before block
	;
	bra	bcopyok
.skip	cmp	bboty(pc),d7
	bhi	bcopyok
	bne	beepem
	bsr	inblokf
	beq	beepem
bcopyok	bsr	bloksetup
	;
	;a5=btopy, d6=btop line
	;d5=btopx
	;a4=thisy, d7=this line
	;
	move.l	chmap(pc),a2
	add	chloc(pc),a2
	move.l	a2,a1
	move	cursoff(pc),d2
	add	cursx(pc),d2
	moveq	#0,d1
	move.l	charbuff2(pc),a0
.loop	cmp	d2,d1
	bcc	.skip2
	move.b	(a1)+,d0
	and.b	#127,d0
	move.b	d0,(a0)+
	addq	#1,d1
	bra	.loop
.skip2	movem.l	d1/a1,-(a7)
.skip	bsr	getbchar
	bmi	.oneline
	beq	.newline
.proc	cmp	chcols(pc),d1
	bcc	.skip
	move.b	d0,(a0)+
	addq	#1,d1
	bra	.skip
.newline	cmp	chcols(pc),d1
	bcc	.newline2
	move.b	#32,(a0)+
	addq	#1,d1
	bra	.newline
.newline2	move.l	charbuff2(pc),a2
	bsr	charstobuff
	move.l	d5,d0
	moveq	#1,d1
	move.l	4,a6
	add	#lenitem,d0
	bsr	alloc2
	move.l	d0,a0
	lea	chars(a0),a0
	move.l	charbuff(pc),a1
.nloop	move.b	(a1)+,(a0)+
	bne	.nloop
	move.l	d0,a0
.insert	add	#1,numlines
	move.b	d5,numchars(a0)
	move.l	lastitem(a4),d1
	bne	.skipz
	move.l	a0,firstitem
	clr.l	lastitem(a0)
	bra	.skipy
.skipz	move.l	d1,a1
	move.l	a0,nextitem(a1)
	move.l	a1,lastitem(a0)
.skipy	move.l	a0,lastitem(a4)
	move.l	a4,nextitem(a0)
	cmp	bboty(pc),d6
	bcc	.skipx
	moveq	#0,d0
	move.b	numchars(a5),d0
	move	d0,d5
	add	#lenitem,d0
	moveq	#1,d1
	move.l	4,a6
	bsr	alloc2
	move.l	d0,a0
	movem.l	a0/a5,-(a7)
	lea	chars(a0),a0
	lea	chars(a5),a5
.loopz	move.b	(a5)+,(a0)+
	bne	.loopz
	movem.l	(a7)+,a0/a5
	addq	#1,d6
	move.l	nextitem(a5),a5
	bra	.insert
.skipx	move	tokenise(pc),d0
	beq	.skipx2
	bsr	detoka5
.skipx2	move.l	charbuff2(pc),a0
	moveq	#0,d5
	moveq	#0,d1
	bra	.skip
	;
.oneline	movem.l	(a7)+,d1/a1
.oneline3	cmp	chcols(pc),d1
	bcc	.oneline2
	move.b	(a1)+,d0
	and.b	#127,d0
	move.b	d0,(a0)+
	addq	#1,d1
	bra	.oneline3
.oneline2	cmp.l	charbuff3(pc),a0
	bcc	.line
	move.b	#32,(a0)+
	bra	.oneline2
.line	bsr	makefirst
	move	cursoff(pc),d0
	add	cursx(pc),d0
	move	curstop(pc),d7
	add	cursy(pc),d7
	cmp	bboty(pc),d7
	bhi	.done2
	bne	.shit
	cmp	bbotx(pc),d0
	bhi	.done2
.shit	;
	;o.k., here we have to adjust the blocks position
	;
	;first, calculate how far down the block has to go
	;into d1
	move	bboty(pc),d1
	sub	btopy(pc),d1
	move	bbotx(pc),d2
	addq	#1,d2
	cmp	chcols(pc),d2
	bcs	.shit2
	addq	#1,d1
	cmp	btopy(pc),d7
	bcs	.shit3
	sub	d0,btopx
	bra	.shit3
.shit2	cmp	btopy(pc),d7
	bcs	.shit3
	cmp	bboty(pc),d7
	beq	.shit5
	move	bbotx(pc),d2
	sub	d0,d2
	addq	#1,d2
	add	d2,btopx
	bra	.shit4
.shit5	move	bbotx(pc),d2
	sub	btopx(pc),d2
	addq	#1,d2
	add	d2,btopx
	add	d2,bbotx
.shit4	bsr	blokfix
.shit3	add	d1,btopy
	add	d1,bboty
.done2	bsr	calcitem
	bra	showall

blokforget	cmp	#-1,bboty
	beq	beepem
	bsr	blokdis
	bra	showall

gomouse	move	edcols(pc),d0
	addq	#2,d0
	cmp	d0,d2
	bcs	.skip
	cmp	nummice(pc),d3
	bcc	.skip
	lea	mice(pc),a0
.loop	move.l	(a0),a0
	dbf	d3,.loop
	move.l	firstitem(pc),a1
	moveq	#0,d0
	move	numlines(pc),d1
	subq	#1,d1
.loop2	cmp.l	4(a0),a1
	beq	gotolined0
	move.l	(a1),a1
	addq	#1,d0
	dbf	d1,.loop2
.skip	rts

mousedown	;o.k. - now for the biggy - select text with the
	;f***ing mouse.
	;
	bsr	getmousexy
	bmi	.done
	cmp	edcols(pc),d2
	bcc	gomouse
	move	d3,d1
	add	curstop(pc),d1
	cmp	numlines(pc),d1
	bcc	.done
	;
	move.l	btopx(pc),-(a7)
	move.l	bbotx(pc),-(a7)
	cmp	#-1,bboty
	beq	.skipsal
	bsr	blokdis
	movem	d2-d3,-(a7)
	bsr	showall
	movem	(a7)+,d2-d3
.skipsal	move	d3,d1
	add	curstop(pc),d1
	move	d1,bloktopy
	move	d1,blokboty
	add	cursoff(pc),d2
	move	d2,bloktopx
	move	d2,blokbotx
	bsr	showprops2
	move	d2,d5
	move	d3,d6
	move	d2,d0
	move	d3,d1
	clr	invflag
	move	curstop(pc),-(a7)
	move	cursoff(pc),-(a7)
	move.l	a4,-(a7)
.loop2	bsr	invertarea
.loop	bsr	getinput2	;bsr	inputor0
	cmp	#-4,d7
	beq	.mouseup
	bsr	getmousexy
	bpl	.skip0
	cmp	topskip(pc),d1
	bcs	.mup
	;
	;mouse drag down
	;
	move	edrows(pc),d3
	move	d3,d0
	add	curstop(pc),d0
	cmp	numlines(pc),d0
	bcc	.loop
	subq	#1,d3
	move	chcols(pc),d2
	move	curstop(pc),d0
	add	edrows(pc),d0
	cmp	numlines(pc),d0
	bcc	.skip2
	movem	d5-d6,-(a7)
	bsr	scrupall
	move.l	nextitem(a4),a4
	move.l	a4,thisitem
	add	#1,curstop
	bsr	showbotitem
	movem	(a7)+,d5-d6
	subq	#1,d6
.ok	move	chcols(pc),d2
	move	edrows(pc),d3
	subq	#1,d3
	move	d3,d1
	add	curstop(pc),d1
	cmp	bloktopy(pc),d1
	bhi	.skip2
	;
	move	d1,-(a7)
	move	d5,d2
	move	d6,d3
	move	chcols(pc),d0
	move	edrows(pc),d1
	subq	#2,d1
	bsr	invertarea
	move	chcols(pc),d5
	move	edrows(pc),d6
	subq	#1,d6
	move	d6,d3
	moveq	#-1,d2
	move	(a7)+,d1
	cmp	bloktopy(pc),d1
	bne	.skip2
	move	bloktopx(pc),d5
	addq	#1,d5
	bra	.skip2
	;
.mup	;mouse drag up! - currently FUCKED
	;
	moveq	#0,d2
	moveq	#0,d3
	move	curstop(pc),d0
	beq	.skip2
	;
	movem	d5-d6,-(a7)
	bsr	scrdownall
	move.l	lastitem(a4),a4
	move.l	a4,thisitem
	subq	#1,curstop
	bsr	showtopitem
	movem	(a7)+,d5-d6
	addq	#1,d6
.ok2	moveq	#0,d3
	moveq	#0,d2
	move	curstop(pc),d1
	cmp	bloktopy(pc),d1
	bcs	.skip2
	move	d5,d2
	move	d6,d3
	moveq	#0,d0
	moveq	#1,d1
	bsr	invertarea
	move	chcols(pc),d2
	moveq	#0,d3
	moveq	#-1,d5
	moveq	#0,d6
	move	curstop(pc),d1
	cmp	bloktopy(pc),d1
	bne	.skip2
	move	bloktopx(pc),d5
	subq	#1,d5
	bra	.skip2
	;
.skip0	add	cursoff(pc),d2
	move	d3,d1
	add	curstop(pc),d1
	cmp	numlines(pc),d1
	bcs	.skip0z
	move	numlines(pc),d3
	sub	curstop(pc),d3
	subq	#1,d3
	;
.skip0z	tst	d0
	bne	.skip0y
	;go left
	subq	#1,d2
	move	cursoff(pc),d0
	beq	.skip2
	subq	#1,d2
	movem	d2-d3/d5-d6,-(a7)
	move	d5,blokbotx
	move	d6,d1
	add	curstop(pc),d1
	move	d1,blokboty
	move.l	bloktopx(pc),btopx
	move.l	blokbotx(pc),bbotx
	bsr	sortblox
	subq	#2,cursoff
	bsr	scrollrite
	bsr	blokdis
	movem	(a7)+,d2-d3/d5-d6
	bra	.skip2
	;
.skip0y	;
	;go rite
	;
	move	d2,d0
	sub	cursoff(pc),d0
	cmp	edcols(pc),d0
	bcs	.skip2
	move	cursoff(pc),d2
	add	edcols(pc),d2
	cmp	chcols(pc),d2
	bcc	.skip2
	addq	#1,d2
	movem	d2-d3/d5-d6,-(a7)
	move	d5,blokbotx
	move	d6,d1
	add	curstop(pc),d1
	move	d1,blokboty
	move.l	bloktopx(pc),btopx
	move.l	blokbotx(pc),bbotx
	bsr	sortblox
	add	#2,cursoff
	bsr	scrollleft
	bsr	blokdis
	movem	(a7)+,d2-d3/d5-d6
	bra	.skip2
	;
.skip2	cmp	d5,d2
	bne	.skip
	cmp	d6,d3
	beq	.loop
.skip	;
	bsr	showprops2
	move	d2,blokbotx
	move	d3,d1
	add	curstop(pc),d1
	move	d1,blokboty
	cmp	bloktopy(pc),d1
	bcs	.above
	bne	.below
	cmp	bloktopx(pc),d2
	blt	.above
.below	move	invflag(pc),d4
	bne	.fix
	move	d5,d0
	move	d6,d1
	move	d2,d5
	move	d3,d6
	bsr	orderem
	addq	#1,d0 
	bsr	invertarea2
	bra	.loop
.above	move	invflag(pc),d4
	beq	.fix
	move	d5,d0
	move	d6,d1
	move	d2,d5
	move	d3,d6
	bsr	orderem
	subq	#1,d2
	bsr	invertarea2
	bra	.loop
.fix	not	invflag
	movem	d2-d3,-(a7)
	move	d5,d2
	move	d6,d3
	move	bloktopx(pc),d0
	move	bloktopy(pc),d1
	sub	curstop(pc),d1
	bpl	.fskip
	moveq	#0,d0
	moveq	#0,d1
	bra	.fskip2
.fskip	cmp	edrows(pc),d1
	bcs	.fskip2
	move	edrows(pc),d1
	subq	#1,d1
	move	chcols(pc),d0
	subq	#1,d0
.fskip2	movem	d0-d1,-(a7)
	bsr	invertarea
	movem	(a7)+,d0-d1
	movem	(a7)+,d2-d3
	move	d2,d5
	move	d3,d6
	bsr	invertarea
	bra	.loop 
.done	rts
.mouseup	move	curstop(pc),newctop
	move	cursoff(pc),newcoff
	move.l	(a7)+,thisitem
	move	(a7)+,cursoff
	move	(a7)+,curstop
	move	blokbotx(pc),d0
	bpl	.shit
	move	blokboty(pc),d1
	cmp	bloktopy(pc),d1
	bls	.shit3
	subq	#1,blokboty
	bra	.shit4
.shit3	moveq	#0,d0
	bra	.shit2
.shit	cmp	chcols(pc),d0
	bcs	.shit2
.shit4	move	chcols(pc),d0
	subq	#1,d0
.shit2	move	d0,blokbotx
	move.l	bloktopx(pc),d0
	move.l	blokbotx(pc),d1
	cmp.l	d1,d0
	beq	.one
	movem.l	d0-d1,-(a7)
	bsr	bloken
	movem.l	(a7)+,d0-d1
	addq	#8,a7
	move.l	d0,btopx
	move.l	d1,bbotx
	bsr	sortblox
	bsr	showhprop
	bra	showall
.one	;they've just selected a one block block -
	;move cursor there.
	;
	move.l	(a7)+,bbotx
	move.l	(a7)+,btopx
	move	bloktopx(pc),d0
	sub	cursoff(pc),d0
	move	d0,cursx
	move	curstop(pc),d0
	add	cursy(pc),d0
	move.l	thisitem(pc),a4
oneloop	cmp	bloktopy(pc),d0
	beq	.skip
	bcs	.skip2
	move.l	lastitem(a4),a4
	subq	#1,d0
	bra	oneloop
.skip2	move.l	nextitem(a4),a4
	addq	#1,d0
	bra	oneloop
.skip	;
	sub	curstop(pc),d0
	move	d0,cursy
	move.l	a4,thisitem
	bsr	calcpos
	cmp	#-1,bboty
	beq	.nben	;no block marked.
	;
	bsr	bloken
	bra	showall
	;
.nben	move	curstop(pc),d0
	cmp	newctop(pc),d0
	bne	showall
	move	cursoff(pc),d0
	cmp	newcoff(pc),d0
	bne	showall
	bra	cursor2
	;
newctop	dc	0
newcoff	dc	0
	
;-----------subroutines below---------------------;	
subs

gotoreq	dc.l	0
	dc.w	140,58,184,40,0,0
	dc.l	gotoreqga1,gotoreqbo1,gotoreqin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

gotoreqbo1	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	gotoreqla1,gotoreqla2
gotoreqla1	dc.w	175,4,120,4,120,15
gotoreqla2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	gotoreqla3,gotoreqbo2
gotoreqla3	dc.w	175,5,175,15,121,15
gotoreqbo2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	gotoreqla4,gotoreqla5
gotoreqla4	dc.w	183,0,0,0,0,39
gotoreqla5	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	gotoreqla6,0
gotoreqla6	dc.w	183,1,183,39,1,39
gotoreqbo3	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	gotoreqla9,gotoreqla10
gotoreqla9	dc.w	39,0,0,0,0,11
gotoreqla10	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	gotoreqla11,0
gotoreqla11	dc.w	39,1,39,11,1,11
gotoreqbo4	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	gotoreqla13,gotoreqla14
gotoreqla13	dc.w	71,0,0,0,0,11
gotoreqla14	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	gotoreqla15,0
gotoreqla15	dc.w	71,1,71,11,1,11

gotoreqin1	dc.b	1,0,1,0
	dc.w	12,6
	dc.l	0
	dc.l	gotoreqla7,0
gotoreqla7	dc.b	'LINE TO GOTO:',0
	even
gotoreqin2	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	gotoreqla12,0
gotoreqla12	dc.b	' OK ',0
	even
gotoreqin3	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	gotoreqla16,0
gotoreqla16	dc.b	' CANCEL ',0
	even

gotoreqga1	dc.l	gotoreqga2
	dc.w	124,6,48,8,0,2049+4,4
	dc.l	0,0,0,0,gotoreqsg1
	dc.w	1
	dc.l	0
gotoreqsg1	dc.l	gotoreqla8,undobuffer
	dc.w	0,6,0
	dc.w	0,0,0,0,0,0,0
gotoint	dc.l	0,0
gotoreqla8	dcb.b	6,0
	even
gotoreqga2	dc.l	gotoreqga3
	dc.w	8,24,40,12,0,5,1
	dc.l	gotoreqbo3,0,gotoreqin2,0,0
	dc.w	2
	dc.l	0
gotoreqga3	dc.l	0
	dc.w	104,24,72,12,0,5,1
	dc.l	gotoreqbo4,0,gotoreqin3,0,0
	dc.w	3
	dc.l	0


actgad	;activate gadget in a0
	;
	move.l	a0,-(a7)
	moveq	#8,d1
	calldos	delay
	move.l	(a7)+,a0
	move.l	req_window(pc),a1
	sub.l	a2,a2
	callint	activategadget
	rts

gotoline	clr	gotoreqsg1+8
	clr	gotoreqsg1+12
	lea	gotoreq(pc),a0
	clr.l	gotoint
	clr.b	gotoreqla8
	bsr	openreq
	lea	gotoreqga1(pc),a0
	lea	gotoreq(pc),a2
	bsr	actgad
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#1,d6
	beq	.ok
	cmp	#2,d6
	beq	.ok
	cmp	#3,d6
	bne	.loop
	rts
.ok	move.l	gotoint(pc),d0
	beq	beepem
	cmp	numlines(pc),d0
	bhi	beepem
	subq	#1,d0
	bra	gotolined0

ioname	dc.b	'NEW'
ioname2	dc.b	'CON:0/0/640/'
hiteasc	dc.b	'   /Blitz 2 CLI Window',0
	even

oldih	dc.l	0
oldoh	dc.l	0
myhandle	dc.l	0

bstx	dc	-1	;no block start
bsty	dc	0

blockstart	;f1 - genam style
	;
	move	cursoff(pc),d0
	add	cursx(pc),d0
	move	d0,bstx
	move	curstop(pc),d0
	add	cursy(pc),d0
	move	d0,bsty
	rts

blockend	;f2 - genam style
	;
	move	bstx(pc),d2		;x start
	bmi	beepem
	move	d0,btopx
	move	bsty(pc),d3		;y start
	move	curstop(pc),d1
	add	cursy(pc),d1	;y end
	move	cursoff(pc),d0
	add	cursx(pc),d0	;x end
	cmp	d0,d2
	bne	.doit
	cmp	d1,d3
	bne	.doit
	bsr	blokdis
	bra	showall
	;
.doit	tst	d0
	bne	.skip
	tst	d1
	beq	.skip
	subq 	#1,d1
	move	chcols(pc),d0
.skip	subq	#1,d0
	move	d0,bbotx
	move	d1,bboty
	move	d2,btopx
	move	d3,btopy
	;
	bsr	sortblox
	bsr	bloken
	bra	showall

closewb	;
	callint	closeworkbench
;	move.l	myscreen(pc),a0
;	move	#1,20(a0)	;Let's be Workbench!
	rts

openwb	callint	openworkbench
	move.l	myscreen(pc),a0
	jsr	screentofront(a6)
	move.l	mywindow(pc),a0
	jmp	activatewindow(a6)

findio	move.l	dos,a1
	move.l	34(a1),a1
	move.l	24(a1),a1
	add.l	a1,a1
	add.l	a1,a1
	move.l	4(a1),d1
	;
.loop	move.l	d1,a1
	add.l	a1,a1
	add.l	a1,a1
	move.l	40(a1),a2
	add.l	a2,a2
	add.l	a2,a2
	cmp.b	(a2)+,d0
	bne	.next
	move.l	a0,a3
	move	d0,d1
	subq	#1,d1
.comp	cmp.b	(a3)+,(a2)+
	bne	.next
	dbf	d1,.comp
	moveq	#0,d0
	rts
.next	move.l	(a1),d1
	bne	.loop
	moveq	#-1,d0
	rts

calciorows	move.l	gr(pc),a1
	move	#200,d0
	btst	#0,207(a1)
	bne	.isntsc
	move	#256,d0
.isntsc	rts

openio	;open a default io handle
	;
	lea	hiteasc(pc),a0
	move.b	#32,(a0)
	move.b	#32,1(a0)
	move.b	#32,2(a0)
	move	newscreen+6(pc),d0
	tst	screensopen
	bne	.docon
	bsr	calciorows
.docon	bsr	numtoascii
	;
	sub.l	a1,a1
	move.l	4.w,a6
	jsr	findtask(a6)
	move.l	d0,a3
	move.l	156(a3),oldih
	move.l	160(a3),oldoh
	;
	move.l	dos(pc),a6
	;
	;see if NEWCON is there...
	;
	lea	ioname(pc),a0
	moveq	#6,d0
	movem.l	a2-a3,-(a7)
	bsr	findio
	movem.l	(a7)+,a2-a3
	beq	.usenew
	;
.useold	move.l	#ioname2,d1
	move.l	#1006,d2
	jsr	open(a6)
	move.l	d0,myhandle
	beq	 .fuct
	bra	.isok
	;	
.usenew	move.l	#ioname,d1
	move.l	#1006,d2
	jsr	open(a6)
	move.l	d0,myhandle
	beq	.fuct
	;
.isok	move.l	d0,156(a3)
	move.l	d0,160(a3)
	;
	callint	wbenchtofront
.fuct	rts

closeio	sub.l	a1,a1
	move.l	4.w,a6
	jsr	findtask(a6)
	move.l	d0,a3
	move.l	oldih(pc),156(a3)
	move.l	oldoh(pc),160(a3)
	move.l	dos(pc),a6
	move.l	myhandle(pc),d1
	jsr	close(a6)
	tst	screensopen
	beq.s	.nosc
	move.l	myscreen(pc),a0
	callint	screentofront
	move.l	mywindow(pc),a0
	jmp	activatewindow(a6)
.nosc	rts

nullcom	dc.l	0

mycli	bsr	openio
	beq	.skip
	;
 	move.l	dos(pc),a6
	move.l	#nullcom,d1
	move.l	myhandle(pc),d2
	moveq	#0,d3
	jsr	execute(a6)
	;
	bra	closeio
	;
.skip	rts

refgads	;refresh gadgets - a0=gadget, d0=number
	;
	move.l	req_window(pc),a1
	sub.l	a2,a2
	move.l	int(pc),a6
	jmp	refreshglist(a6)

tokone	;tokenise one line - line in a0
	;
	move.l	a0,a2
	moveq	#0,d1
.loop	addq	#1,d1
	tst.b	(a0)+
	bne	.loop
	move.l	a2,a1
	subq	#1,d1
	bpl	tokit2
	rts

tokall	;tokenise entire source
	;
	move	tokenise(pc),d0
	bne	beepem
	not	tokenise
tokall2	move.l	firstitem(pc),a4
	move	numlines(pc),-(a7)
.loop	moveq	#0,d1
	move.b	numchars(a4),d1
	subq	#1,d1
	bmi	.noway
	lea	chars(a4),a2
	move.l	charbuff(pc),a1
	bsr	tokit2
	move.l	a4,thisitem
	bsr	makeline
.noway	move.l	nextitem(a4),a4
	subq	#1,(a7)
	bne	.loop
	addq	#2,a7
	bra	detokallc

detokallz	move	tokenise(pc),d0
	beq	beepem
	clr	tokenise
detokallz2	move.l	firstitem(pc),a4
	move	numlines(pc),-(a7)
.loop	lea	chars(a4),a0
	move.l	charbuff(pc),a1
	bsr	detok
	sub.l	charbuff(pc),a1
	addq	#1,a1
	move.l	a1,d5
	move.l	a4,thisitem
	bsr	makeline
	move.l	nextitem(a4),a4
	subq	#1,(a7)
	bne	.loop
	addq	#2,a7
	rts

detokall	move	tokenise(pc),d0
	beq	beepem	;already done.....
	clr	tokenise
detokall2	bsr	detokallz2
	;
detokallc	bsr	calcitem
	bra	showall

help	;the help key stuff.....
	;
	move	tokenise(pc),d0
	beq	beepem
	lea	chars(a4),a4	;a4=start of line
	move	cursoff(pc),d3
	add	cursx(pc),d3	;up to....
	moveq	#0,d1		;no token found yet
	moveq	#0,d2		;chk x
.loop	moveq	#0,d0
	move.b	(a4)+,d0
	beq	.done
	bpl	.next
	;
	lsl	#8,d0
	move.b	(a4)+,d0
	;
	move	d0,helptoke
	;
	bsr	findtoke
	move.l	a3,d1
.loop2	addq	#1,d2
	tst.b	(a3)+
	bne	.loop2
	move.l	a3,a2
	subq	#2,d2
.next	addq	#1,d2
	cmp	d3,d2
	bls	.loop
.done	tst.l	d1
	beq	showfname
	;
	move.l	d1,a0
	lea	newsctit(pc),a1
.loop3	move.b	(a0)+,(a1)+
	bne	.loop3
	subq	#1,a1
	cmp.b	#'(',(a2)
	beq	.loop4
	move.b	#32,(a1)+
.loop4	move.b	(a2)+,(a1)+
	bne	.loop4
	move.l	myscreen(pc),a0
	moveq	#-1,d0
	move.l	int(pc),a6
	jsr	showtitle(a6)
	;
	move	qualifier(pc),d0
	and	#$80,d0
	bne.s	.am
	rts
.am	lea	helphex(pc),a1
	move	helptoke(pc),d0
	moveq	#3,d1
.looph	rol.w	#4,d0
	move	d0,d2
	and	#15,d2
	add	#48,d2
	cmp	#58,d2
	bcs.s	.notal
	addq	#7,d2
.notal	move.b	d2,(a1)+
	dbf	d1,.looph
	;
	move.l	#helpex,d1
	moveq	#0,d2
	moveq	#0,d3
	move.l	dos(pc),a6
	jmp	execute(a6)

helptoke	dc	0
helpex	dc.b	'blitz2:help '
helphex	dc.b	'1234',0

doclean	move.l	d0,-(a7)
	move.l	cleanup(pc),d0
	beq	.skip
	movem.l	d1-d7/a0-a6,-(a7)
	move.l	d0,a0
	jsr	(a0)
	movem.l	(a7)+,d1-d7/a0-a6
.skip	move.l	(a7)+,d0
	rts

getstuff	move.l	tokens(pc),d0
	beq	.skip
	move	#1,tokenise
	or	#$80,defaultreqga8+12
.skip	move.l	menitems(pc),d0
	beq	.skip2
	move.l	d0,a5
	move.l	mywindow(pc),a0
	move.l	int(pc),a6
	jsr	clearmenustrip(a6)
	move.l	4,a6
	moveq	#menu2name-menu2,d0
	moveq	#1,d1
	jsr	allocmem(a6)
	move.l	d0,a0
	lea	menu2(pc),a1
	move.l	a0,(a1)
.loop2	move	(a1)+,(a0)+
	cmp.l	#menu2name,a1
	bcs	.loop2
	move.l	d0,a0
	clr.l	(a0)
	move.l	a5,14(a0)
	moveq	#0,d0
.loop3	addq	#1,d0
	tst.b	(a5)+
	bne	.loop3
	move	#192+56,4(a0)
	lsl	#3,d0
	move	d0,8(a0)
	lea	18(a0),a4
	moveq	#0,d7
.loop4	tst.b	(a5)
	beq	.skip3
	moveq	#m2i1text-menu2item1,d0
	moveq	#1,d1
	jsr	allocmem(a6)
	move.l	d0,a0
	lea	menu2item1(pc),a1
.loop5	move	(a1)+,(a0)+
	cmp.l	#m2i1text,a1
	bcs	.loop5
	move.l	d0,a0
	;
	move	#200,8(a0)	;!*!
	;
	clr.l	(a0)
	move	d7,6(a0)
	add	#12,d7
	move.l	a0,(a4)
	move.l	a0,a4
	lea	34(a0),a1
	move.l	a1,18(a0)
	move.l	a5,46(a0)
.loop6	tst.b	(a5)+
	bne	.loop6
	clr.b	26(a0)
	and	#$ffff-4,12(a0)
	move.b	(a5)+,d0
	beq	.loop4
	move.b	d0,26(a0)
	or	#4,12(a0)
	bra	.loop4
.skip3	move.l	mywindow(pc),a0
	lea	menu1(pc),a1
	move.l	int(pc),a6
	jmp	setmenustrip(a6)
.skip2	rts

makemice	;scan entire program for mousables!
	move.l	firstitem(pc),a0
	move	numlines(pc),d0
	subq	#1,d0
	move.b	label(pc),d1
	;
	move.l	a1,-(a7)
.loop	lea	chars(a0),a1
.loop2	cmp.b	#32,(a1)+
	beq	.loop2
	cmp.b	-(a1),d1
	bne	.skip
	move	d1,mousealt
	bsr	remember2
.skip	move.l	(a0),a0
	dbf	d0,.loop
	move.l	(a7)+,a1
	rts

freemice	move	nummice(pc),d2
	beq	.skip
	move	d2,mousealt
	subq	#1,d2
	move.l	mice(pc),a2
	move.l	4,a6
.loop	moveq	#8,d0
	move.l	a2,a1
	move.l	(a2),a2
	jsr	freemem(a6)
	dbf	d2,.loop
	clr	nummice
.skip	rts

remember	;remember a0 line for mousables
	move	#-1,mousealt
	rts

remember2	movem.l	a1-a2/d0-d1,-(a7)
	moveq	#8,d0
	moveq	#1,d1
	move.l	4,a6
	move.l	a0,-(a7)
	jsr	allocmem(a6)
	tst.l	d0
	beq	outmem
	move.l	(a7)+,a0
	move.l	d0,a2
	move	nummice(pc),d1
	beq	.skip
	move.l	mice(pc),a1
	subq	#2,d1
	bmi	.skip3
.loop	move.l	(a1),a1
	dbf	d1,.loop
.skip3	move.l	a2,(a1)
	bra	.skip2
.skip	move.l	a2,mice
.skip2	move.l	a0,4(a2)
	add	#1,nummice
	movem.l	(a7)+,d0-d1/a1-a2
	rts

mforget2	;forget a1 if in mouse list
	move	#-1,mousealt
	rts

showmice	;show all mouseable labels!
	cmp.b	#255,label
	bne	.skip
	rts
.skip	bsr	freemice
	bsr	makemice
	move	nummice(pc),d3
	bne.s	.ismice
	;
	tst	mousecols
	beq.s	.miceok2
	clr	mousecols
	bsr	screenclose
	bra	screenopen2
.miceok2	rts
	;
.ismice	tst	mousecols
	bne.s	.miceok1
	move	#12,mousecols
	bsr	screenclose
	bsr	screenopen2
	move	nummice(pc),d3
	;
.miceok1	move.l	bitmaps+4(pc),a3
	add	edcols(pc),a3
	addq	#2,a3
	moveq	#0,d4
	move.l	mice(pc),a4
.loop	move	mousecols(pc),d5
	subq	#1,d5
	cmp	d3,d4
	bcc	.loop3
	move.l	4(a4),a2
	lea	chars(a2),a2
	move	tokenise(pc),d0
	beq	.loop12
	move.l	a2,a0
	move.l	charbuff(pc),a1
	move.l	a3,-(a7)
	bsr	detok
	move.l	(a7)+,a3
	move.l	charbuff(pc),a2
.loop12	cmp.b	#32,(a2)+
	beq	.loop12
	;
.loop2	move.b	(a2)+,d0
	beq	.loop3
	cmp.b	colon(pc),d0
	beq	.loop3
	cmp.b	#32,d0
	beq	.loop3
	;
	move.l	a3,a0
	move.l	a2,-(a7)
	bsr	qprint
	move.l	(a7)+,a2
	addq	#1,a3
	dbf	d5,.loop2
	bra	.next
.loop3	move.b	#32,d0
	move.l	a3,a0
	bsr	qprint
	addq	#1,a3
	dbf	d5,.loop3
.next	sub	mousecols(pc),a3
	add	fontmul(pc),a3
	cmp	#0,a4
	beq.s	.skip0
	move.l	(a4),a4
.skip0	addq	#1,d4
	cmp	edrows(pc),d4
	bcs	.loop
	rts

a0toname	;convert a0 to a path/filename
	lea	filepath(pc),a1
	move.l	a1,a2	
	cmp.b	#'"',(a0)
	bne	.loop
	addq	#1,a0
.loop	move.b	(a0)+,(a1)+
	beq	.done
	cmp.b	#':',d0
	beq	.skip2
	cmp.b	#'/',d0
	bne	.loop
	lea	-1(a1),a2
	bra	.loop
.skip2	move.l	a1,a2
	bra	.loop
.done	lea	filename(pc),a1
	move.l	a2,a0
	cmp.b	#'/',(a2)
	bne	.loop2
	addq	#1,a2
.loop2	move.b	(a2)+,(a1)+
	bne	.loop2
	cmp.b	#'"',-2(a1)
	bne	.sskip
	clr.b	-2(a1)
.sskip	clr.b	(a0)
	rts

printmem	move.l	4,a6
	move.l	#1<<17+1,d1
	jsr	availmem(a6)
	lsr.l	#8,d0
	lsr.l	#2,d0
	move.l	#$20202020,numtext
	move	#$2000,numtext+4
	lea	numtext(pc),a0
	bsr	numtoascii
	move.l	statsloc(pc),a1
	lea	38(a1),a1
	lea	numtext(pc),a3
	bra	pstats	

alloc2	move	#-1,memalt
	jsr	allocmem(a6)
	tst.l	d0
	beq	outmem
	rts
outmem	bsr	doalert
	move.l	filehand(pc),d1
	beq	.skip
	calldos	close
.skip	clr	curstop
	clr	cursy
	move.l	firstitem(pc),thisitem
	bsr	showall
	move.l	memstack(pc),a7
	bra	memcont

extalloc	;external allocmem
	jsr	allocmem(a6)
	tst.l	d0
	bne	.done
	bsr	closereq
	bra	outmem
.done	rts

doalert	moveq	#0,d0
	lea	alertstuff(pc),a0
	moveq	#40,d1
	move.l	int(pc),a6
	jmp	displayalert(a6)

loaddefs	;attempt to load defaults from l:blitzeditor.opts
	move.l	#defname,d1
	move.l	#1005,d2
	calldos	open
	move.l	d0,d7
	beq.s	.skip
	move.l	d7,d1
	move.l	#defstart,d2
	move.l	#defend-defstart,d3
	jsr	read(a6)
	move.l	d7,d1
	jsr	close(a6)
.skip	lea	defstart(pc),a5
	bra	setdefs

savedefs	;attempt to save defaults to l:blitzeditor.opts
	move.l	#defname,d1
	move.l	#1006,d2
	calldos	open
	move.l	d0,d7
	beq	.skip
	move.l	d7,d1
	move.l	#defstart,d2
	move.l	#defend-defstart,d3
	jsr	write(a6)
	move.l	d7,d1
	jmp	close(a6)
.skip	rts

setdefs	;convert options at a5 to defaults
	;
	move.l	tokens(pc),d0
	bne	.skip0
	clr	6(a5)
.skip0	and	#$ffff-$80,defaultreqga7+12
	cmp	#6,(a5)
	bne	.skip
	or	#$80,defaultreqga7+12
.skip	and	#$ffff-$80,defaultreqga8+12
	move	6(a5),d0
	lsl	#7,d0
	or	d0,defaultreqga8+12
	and	#$ffff-$80,defaultreqga9+12
	move	8(a5),d0
	lsl	#7,d0
	or	d0,defaultreqga9+12
	lea	10(a5),a1
	lea	normcol(pc),a2
	moveq	#0,d2
.loop	move	(a2)+,d0
	or	d2,d0
	bsr	swapd0bord
	move	(a1)+,d0
	or	d2,d0 
	bsr	swapd0bord
	addq	#4,d2
	cmp	#16,d2
	bcs	.loop
	move	18(a5),d0
	lea	defaultreqla47(pc),a0
	bsr	numtoascii2
	move	20(a5),d0
	lea	defaultreqla48(pc),a0
	bsr	numtoascii2
	move	22(a5),d0
	lea	defaultreqla49(pc),a0
	bsr	numtoascii2
	lea	defstart(pc),a0
	moveq	#(defend-defstart)/2-1,d0
.loop2	move	(a5)+,(a0)+
	dbf	d0,.loop2
	move	hscmarg,int1+2
	move	vscmarg,int2+2
	move	tabstop,int3+2
	move	propcol(pc),d0
	bra	setprops

numtoascii2	bsr	numtoascii
	clr.b	(a0)
	rts

numtoascii	;convert number to ascii
	;numbere to d0, ascii buffer in a0
	and.l	#$ffff,d0
	moveq	#0,d1	;printed anything flag
	lea	powersof10(pc),a1
	moveq	#4,d2
.loop	divu	(a1)+,d0
	tst	d0
	bne	.skip
	tst	d1
	beq	.next
.skip	moveq	#1,d1
	add	#48,d0
	move.b	d0,(a0)+
	clr	d0
.next	swap	d0
	dbf	d2,.loop
	tst	d1
	bne	.skip2
	move.b	#48,(a0)+
.skip2	rts
powersof10	dc	10000,1000,100,10,1

setprops	;set prop settings to colour reg d0
	lsl	#1,d0
	move	d0,d1
	lsl	#1,d0
	add	d1,d0
	lea	palit(pc),a0
	add	d0,a0
	lea	defaultreqhp1+2(pc),a1
	moveq	#2,d1
.loop	moveq	#-1,d2
	move	(a0)+,d0
	cmp	#15,d0
	beq	.skip
	swap	d0
	clr	d0
	divu	#15,d0
	move	d0,d2
.skip	move	d2,(a1)
	lea	defaultreqhp2-defaultreqhp1(a1),a1
	dbf	d1,.loop
	rts

dispprops	lea	defaultreqga1(pc),a0
	move.l	req_window(pc),a1
	lea	defaultreq(pc),a2
	moveq	#3,d0
	move.l	int(pc),a6
	jmp	refreshglist(a6)

propadj	;do setrgb4 if necessary
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d3
	lea	defaultreqhp1+2(pc),a0
	move	(a0),d1
	lsr	#8,d1
	lsr	#4,d1
	lea	defaultreqhp2+2(pc),a0
	move	(a0),d2
	lsr	#8,d2
	lsr	#4,d2
	lea	defaultreqhp3+2(pc),a0
	move	(a0),d3
	lsr	#8,d3
	lsr	#4,d3
	cmp	(a5),d1
	bne	.doit
	cmp	2(a5),d2
	bne	.doit
	cmp	4(a5),d3
	beq	.skip
.doit	move	d1,(a5)
	move	d2,2(a5)
	move	d3,4(a5)
	move.l	d4,d0
	move.l	myscreen(pc),a0
	lea	44(a0),a0
	move.l	gr(pc),a6
	jmp	setrgb4(a6)
.skip	rts

dispgadd0	;display gadget d0
	lea	defaultreqga10(pc),a0
	mulu	#defaultreqga11-defaultreqga10,d0
	add	d0,a0
	move.l	req_window(pc),a1
	lea	defaultreq(pc),a2
	moveq	#1,d0
	move.l	int(pc),a6
	jmp	refreshglist(a6)

swapd0bord	mulu	#defaultreqbo15-defaultreqbo14,d0
	lea	defaultreqbo14(pc),a0
	bsr	.skip
	lea	defaultreqla63-defaultreqbo14(a0),a0
.skip	move.b	4(a0,d0),d1
	move.b	5(a0,d0),4(a0,d0)
	move.b	d1,5(a0,d0)
	rts

swapbords	;
	lea	defaultreqbo14(pc),a0
	moveq	#31,d0
.loop	move.b	4(a0),d1
	move.b	5(a0),4(a0)
	move.b	d1,5(a0)
	lea	defaultreqla63-defaultreqbo14(a0),a0
	dbf	d0,.loop
	rts

erase	bsr	erase2
	beq	.skip
	addq	#4,a7
.skip	rts

erase2	;check erase request. Return eq if OK
	move	allalt(pc),d0
	beq	.skip
	lea	erasereq(pc),a0
	bsr	openreq
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#1,d6
	beq	.skip
	rts
.skip	move	#-1,mousealt
	cmp	d6,d6
	rts

linka0	;add a0 after a4
	;
	add	#1,numlines
	move.l	nextitem(a4),d0
	beq	.end
	move.l	d0,a1
	move.l	a1,nextitem(a0)
	move.l	a0,lastitem(a1)
	bra	.end2
.end	move.l	a0,enditem
.end2	move.l	a4,lastitem(a0)
	move.l	a0,nextitem(a4)
	move.l	a0,a4
	move.l	a4,thisitem
	rts

bloadadd	move.l	charbuff2(pc),a0
.loop	move.b	(a0)+,(a3)+
	beq	.done
	bpl	.loop
	move.b	(a0)+,(a3)+
	bra	.loop
.done	subq	#1,a3
	rts

doclose	clr.l	filehand
	move.l	d7,d1
	move.l	dos(pc),a6
	jmp	close(a6)

doopen	;open the file and fill buffer with bytes
	;filename in d7, mode in d2!
	;
	;
	;use a5 for bufferpointer
	;uses d6 to keep track of characters left in dosbuff
	;uses d7 for filehandle
	;
	move.l	d7,d1
	calldos	open
	move.l	d0,d7
	bne	.skip
	addq	#4,a7
	lea	error1(pc),a0
	bra	error
.skip	move.l	d7,filehand
	rts

doread	;get next character from file (or <0 if eof!)
	;
.skip2	subq	#1,d6
	bmi	.skip
	move.b	(a5)+,d0
	cmp	d0,d0
	rts
.skip	bsr	fillbuff
	bne	.skip2
	moveq	#-1,d0
	rts

fillbuff	move.l	d7,d1
	move.l	dosbuff(pc),a5
	move.l	a5,d2
	move.l	#doslen,d3
	calldos	read
	move.l	d0,d6
	bpl	.skip
	lea	error2(pc),a0
	bsr	error
	moveq	#0,d6
.skip	rts

allocline	move.l	d5,d0
	add	#lenitem,d0
	moveq	#1,d1
	move.l	4,a6
	bsr	alloc2
	move.l	d0,a0
	clr.l	nextitem(a0)
	move.b	d5,numchars(a0)
	move.l	charbuff2(pc),a1
	lea	chars(a0),a0
.loop	move.b	(a1)+,(a0)+
	bne	.loop	
	move.l	d0,a0
	move.b	label(pc),d0
	;
	move.l	a1,-(a7)
	lea	chars(a0),a1
.loop2	cmp.b	#32,(a1)+
	beq	.loop2
	cmp.b	-(a1),d0
	move.l	(a7)+,a1
	;
;	cmp.b	chars(a0),d0
	beq	remember
	rts

readline	;read in a line to charbuff2
	;
	;return mi if no line to read, 0 if no eol.
	;
	;length in d5
	bsr	doread
	bmi	.skip
	move.l	charbuff2(pc),a0
	moveq	#0,d4	;flag for no toke on load!
.loop	tst.b	d0
	bpl	.tryout
	move.b	d0,(a0)+
	move.l	a0,-(a7)
	bsr	doread
	move.l	(a7)+,a0
	bmi	.oops
	move.b	d0,(a0)+
	bra	.nottab
.tryout	cmp.b	#10,d0
	bne	.skip2
	moveq	#0,d0	;file is in ascii format
	moveq	#-1,d4	;flag it for a toke-up
.skip2	move.b	d0,(a0)+
	beq	.skip3
	cmp.b	#9,d0
	bne	.nottab
	subq	#1,a0
	move.l	a0,d0
	sub.l	charbuff2(pc),d0
.tabloop	move.b	#32,(a0)+
	addq	#1,d0
	move.l	d0,d1
	divu	tabstop(pc),d1
	swap	d1
	tst	d1
	bne	.tabloop
.nottab	cmp.l	charbuff3(pc),a0
	bcs	.ok
	move.l	charbuff3(pc),a0
	subq	#3,a0
	bra	.oops
.ok	move.l	a0,-(a7)
	bsr	doread
	move.l	(a7)+,a0
	bpl	.loop
.oops	clr.b	(a0)+
	sub.l	charbuff2(pc),a0
	move.l	a0,d5
	moveq	#0,d0
.sit	;should i toke it?
	move	d0,-(a7)
	tst	d4
	beq	.no
	move	tokenise(pc),d2
	beq	.no
	;
	;Yup!
	;
	subq	#1,d5
	bpl	.oktt
	moveq	#1,d5
	bra	.no
.oktt	bsr	mytoke
.no	move	(a7)+,d0	
	rts 
.skip3	sub.l	charbuff2(pc),a0
	move.l	a0,d5
	moveq	#1,d0
	bra	.sit
.skip	moveq	#-1,d0
	rts

mytoke	movem.l	d1-d4/d6-d7/a0-a6,-(a7)
	move	d5,d1	;length
	move.l	charbuff2(pc),a1
	move.l	a1,a2
	bsr	tokit2
	movem.l	(a7)+,d1-d4/d6-d7/a0-a6
	rts

detok	;detokenise a0 to a1
	moveq	#0,d0
	move.b	(a0)+,d0
	beq	.done
	bmi	.skip
	move.b	d0,(a1)+
	bra	detok
.skip	lsl	#8,d0
	move.b	(a0)+,d0
	bsr	findtoke
.skip3	move.b	(a3)+,(a1)+
	bne	.skip3
	subq	#1,a1
	bra	detok
.done	clr.b	(a1)
	rts

findtoke	move.l	tokens(pc),a3
	bclr	#15,d0
.loop	cmp	4(a3),d0
	beq	.got
	tst.l	(a3)
	beq	.notoke
	move.l	(a3),a3
	bra	.loop
.got	addq	#6,a3
	rts
.notoke	lea	tokeerr(pc),a3
	rts

getbname	lea	blokpath(pc),a0
	lea	blokname(pc),a1
	bra	getfname3

getfname	lea	filetext(pc),a2
getfname2	lea	filepath(pc),a0
	lea	filename(pc),a1
	;
getfname3	bsr	ezfreq
	beq	.skip
	move.l	d0,d7
	rts
.skip	cmp	#1,d0
	beq	outmem
	addq	#4,a7
	rts

ezfreq	lea	reqstruct(pc),a3
	move.l	a0,(a3)+
	move.l	a1,(a3)+
	move.l	a2,(a3)+
	lea	reqstruct(pc),a0
	bra	filerequest+36

reqstruct	dc.l	0,0,0,0

error	move.l	d7,-(a7)
	move.l	a0,errorpnt
	lea	errorreq(pc),a0
	bsr	openreq
.loop	bsr	getinput
	cmp	#-1,d7
	bne	.loop
	cmp	#1,d6
	bne	.loop
	move.l	(a7)+,d7
	rts

isithere	moveq	#0,d5
	;test if find buffer is in line at a4
	;
	;return d0<0 if not, else at start position
	;
	;d5 = start position for search
	;
isithere2	lea	chars(a4),a5
	move	tokenise(pc),d0
	beq	.skip
	move.l	a5,a0
	move.l	charbuff(pc),a1
	move.l	a1,a5
	bsr	detok
.skip	move.l	a5,a1
	add	d5,a5
.loop0	move.l	a5,a3
	lea	findreqla16(pc),a2
.loop	move.b	(a2)+,d0
	beq	.found
	btst	#7,findreqga8+13
	bne	.skip2
	bsr	alphad0
	bne	.skip2
	and	#255-32,d0
.skip2	move.b	(a3)+,d2
	beq	.notfound
	btst	#7,findreqga8+13
	bne	.skip3
	bsr	alpha
	bne	.skip3
	and	#255-32,d2
.skip3	cmp.b	d0,d2
	beq	.loop
	addq	#1,a5
	bra	.loop0
.found	sub.l	a1,a5
	move	a5,d0
	rts
.notfound	moveq	#-1,d0
	rts

reqwindow	dc.w	0,0,0,0	;left,top,width,height
	dc.b	1,2	;detailpen,blockpen
	dc.l	32+64+$200+$400	;idcmp
	dc.l	2+8+$1000	;flags
req_gads	dc.l	0	;gadgets
	dc.l	0	;checkmark image
	dc.l	0	;title
req_screen	dc.l	0	;screen
	dc.l	0	;bitmap
	dc.w	-1,-1,-1,-1	;minw,maxw etc
	dc.w	15	;type

req_old	dc.l	0
req_old2	dc.l	0

openreq	;open a requester
	;
	;return 0 if couldn't open it!
	;
	move.l	a0,req
	;
	move.l	8(a0),reqwindow+4	;w,h
	add	#12,reqwindow+4
	add	#15,reqwindow+6
	;
	move	edcols(pc),d0
	lsl	#3,d0
	sub	reqwindow+4(pc),d0
	lsr	#1,d0
	move	d0,reqwindow
	;
	move	newscreen+6(pc),d0
;	move	edrows(pc),d0
;	mulu	fonthite(pc),d0
	sub	reqwindow+6(pc),d0
	lsr	#1,d0
	move	d0,reqwindow+2
	;
	move.l	16(a0),a1
	move.l	a1,req_gads
	tst	40(a1)
	bne.s	.skip
	move	#-1,40(a1)
	;
.loop	addq	#6,4(a1)
	add	#12,6(a1)
	move.l	(a1),d0
	move.l	d0,a1
	bne.s	.loop
	;
.skip	move.l	myscreen(pc),req_screen
	;
	lea	reqwindow(pc),a0
	callint	openwindow
	tst.l	d0
	beq	outmem
	;
	move.l	req_old(pc),req_old2
	move.l	req_window(pc),req_old
	move.l	d0,req_window
	;
	move.l	req_window(pc),a0
	move.l	50(a0),a0	;rastport
	move.l	req(pc),a1
	move.l	24(a1),a1	;intuitext
	moveq	#6,d0
	moveq	#12,d1
	move.l	a0,-(a7)
	jsr	printitext(a6)
	move.l	(a7)+,a0	;rastport
	move.l	req(pc),a1
	move.l	20(a1),a1	;border stuff
	moveq	#6,d0
	moveq	#12,d1
	jsr	drawborder(a6)
	;
	bra	pointeron

closereq	movem.l	d0-d1/a0-a2/a6,-(a7)
	;
	move.l	req_window(pc),d0
	beq.s	.done2
	;
	move.l	d0,a2
	move.l	4,a6
	move.l	86(a2),a2
.loop	move.l	a2,a0
	jsr	getmsg(a6)
	tst.l	d0
	beq.s	.done
	move.l	d0,a1
	jsr	replymsg(a6)
	bra.s	.loop
.done	move.l	req_window(pc),a0
	callint	clearpointer
	move.l	req_window(pc),a0
	jsr	closewindow(a6)
	move.l	req_old(pc),req_window
	move.l	req_old2(pc),req_old
	clr.l	req_old2
	;
	bsr	flushmw
	;
.done2	bsr	makemenus
	movem.l	(a7)+,d0-d1/a0-a2/a6
	rts

flushmw	move.l	mywindow(pc),a2
	move.l	4,a6
	move.l	86(a2),a2
.loop	move.l	a2,a0
	jsr	getmsg(a6)
	tst.l	d0
	beq.s	.done
	move.l	d0,a1
	jsr	replymsg(a6)
	bra.s	.loop
.done	rts

freeupa5	;unlink a5 from list
	cmp	#1,numlines
	beq	.freeonly
	sub	#1,numlines
	move.l	nextitem(a5),d1
	move.l	lastitem(a5),d2
	beq	.first
	tst.l	d1
	beq	.last
	move.l	d2,a0
	move.l	d1,nextitem(a0)
	move.l	d1,a0
	move.l	d2,lastitem(a0)
	bra	.free
.first	move.l	d1,firstitem
	move.l	d1,a0
	clr.l	lastitem(a0)
	bra	.free
.last	move.l	d2,enditem
	move.l	d2,a0
	clr.l	nextitem(a0)
.free	move.l	a5,a1
	move.b	label(pc),d0
	;
	move.l	a0,-(a7)
	lea	chars(a1),a0
.loop	cmp.b	#32,(a0)+
	beq	.loop
	cmp.b	-(a0),d0
	move.l	(a7)+,a0
	;
;	cmp.b	chars(a1),d0
	bne	.fa5s
	bsr	mforget2
.fa5s	moveq	#0,d0
	move.b	numchars(a1),d0
	add	#lenitem,d0
	move.l	4,a6
	move	d0,memalt
	jmp	freemem(a6)
.freeonly	bsr	afirst2
	bra	.free

bloksetup2	;
	;a5=btopy, d6=btop line
	;d5=btopx
	;a4=thisy, d7=this line
	;
	;find first item of block
	;
	move	curstop(pc),d7
	add	cursy(pc),d7
	move.l	a4,a5
	move	d7,d6
	cmp	btopy(pc),d6
	beq	found
	bcc	.up
.down	move.l	nextitem(a5),a5
	addq	#1,d6
	cmp	btopy(pc),d6
	bne	.down
	bra	found
.up	move.l	lastitem(a5),a5
	subq	#1,d6
	cmp	btopy(pc),d6
	bne	.up
found	rts

bloksetup	bsr	bloksetup2
	move	btopx(pc),d5
	move	tokenise(pc),d0
	beq	.hendrix
	bra	detoka5
.hendrix	rts
	
detoka5	;
	move.l	charbuff3(pc),a0
	lea	chars(a5),a1
	addq	#1,a0
	bsr	.loop
	move.l	a0,d0
	sub.l	charbuff3(pc),d0
	move.l	charbuff3(pc),a0
	move.b	d0,(a0)
	rts
.loop	move.b	(a1)+,d0
	beq	.done
	bmi	.skip
	move.b	d0,(a0)+
	bra	.loop
.skip	lsl	#8,d0
	move.b	(a1)+,d0
	bsr	findtoke
.found	move.b	(a3)+,(a0)+
	bne	.found
	subq	#1,a0
	bra	.loop
.done	rts

calcitem	;work out item address
	move.l	firstitem(pc),a4
	move	curstop(pc),d0
	add	cursy(pc),d0
	beq	.done
	subq	#1,d0
.loop	move.l	nextitem(a4),a4
	dbf	d0,.loop	
.done	move.l	a4,thisitem
	rts

makeline2	move.l	a3,d5
	sub.l	charbuff(pc),d5
	addq.l	#1,d5
	bra	makeline

makefirst	;
	move.l	charbuff2(pc),a2
	bsr	charstobuff
	;
makeline	;make charbuff current line (this item - a4, len in d5)
	;
	move.l	d5,d0
	add	#lenitem,d0
	moveq	#1,d1
	move.l	4,a6
	bsr	alloc2
	move.l	d0,a0
	move.l	charbuff(pc),a1
	move.b	label(pc),d0
	;
	move.l	a1,-(a7)
.loop	cmp.b	#32,(a1)+
	beq	.loop
	cmp.b	-(a1),d0
	move.l	(a7)+,a1
	;
;	cmp.b	(a1),d0
	bne	.skipz
	bsr	remember
.skipz	cmp.l	firstitem(pc),a4
	bne	.skip2
	move.l	a0,firstitem
.skip2	cmp.l	enditem(pc),a4
	bne	.skip3
	move.l	a0,enditem
.skip3	move.b	d5,numchars(a0)
	move.l	lastitem(a4),d1
	beq	.skip4
	move.l	d1,a1
	move.l	a0,nextitem(a1)
.skip4	move.l	d1,lastitem(a0)
	move.l	nextitem(a4),d1
	beq	.skip5
	move.l	d1,a1
	move.l	a0,lastitem(a1)
.skip5	move.l	d1,nextitem(a0)
	move.l	a0,a4
	lea	chars(a0),a0
	move.l	charbuff(pc),a1
.loop2	move.b	(a1)+,(a0)+
	bne	.loop2
	bsr	freethis
	move.l	a4,thisitem
	rts

getbchar	;get next char from block. 	
	;
	;set mi flag if end of block reached!
	;
	;set eq flag if end of line hit!
	;
	;a5=item, d6=y, d5=x
	cmp	chcols(pc),d5
	bge	newline
	cmp	bboty(pc),d6
	bcs	getbchar2
	bne	.eob
	cmp	bbotx(pc),d5
	bls	getbchar2
.eob	moveq	#-1,d0
	rts
getbchar2	addq	#1,d5
	move	tokenise(pc),d0
	beq	.skipz
	move.l	charbuff3(pc),a3
	cmp.b	(a3),d5
	bcc	.space
	moveq	#0,d0
	move.b	0(a3,d5),d0
	tst	d0	;make plus!
	rts
.skipz	cmp.b	numchars(a5),d5
	bcc	.space
	moveq	#0,d0
	move.b	chars-1(a5,d5),d0
	rts
.space	move	blokop(pc),d0
	bne	newline
	moveq	#32,d0
	rts
newline	move.l	nextitem(a5),a5
	addq	#1,d6
	moveq	#0,d5
	moveq	#0,d0
	rts

bloken	;enable block menu items
	or	#16,menu4item1+12
	or	#16,menu4item2+12
	or	#16,menu4item4+12
	or	#16,menu4itema+12
	or	#16,menu4itemb+12
	lea	bltext(pc),a3
	move.l	#'bloc',(a3)
	move.b	#'k',4(a3)
blokprnt2	move.l	statsloc(pc),a1
	lea	44(a1),a1
	bra	pstats

blokdis	;disable above
	move	#-1,bstx
	and	#$ffff-16,menu4item1+12
	and	#$ffff-16,menu4item2+12
	and	#$ffff-16,menu4item4+12
	and	#$ffff-16,menu4itema+12
	and	#$ffff-16,menu4itemb+12
	move	#-1,bboty
	lea	bltext(pc),a3
	move.l	#'    ',(a3)
	move.b	#' ',4(a3)
	movem.l	d0/a0-a2,-(a7)
	bsr	blokprnt2
	movem.l	(a7)+,d0/a0-a2
	rts

blokadj	cmp	#-1,bboty
	beq	.skip
	cmp	bboty(pc),d0
	bhi	.skip
	addq	#1,bboty
	cmp	btopy(pc),d0
	bhi	.skip
	addq	#1,btopy
.skip	rts

showprops2	move.l	cursx(pc),-(a7)
	movem	d2-d3/d5,-(a7)
	sub	cursoff(pc),d2
	bpl	.hi
	moveq	#0,d2
	bra	.hi2
.hi	cmp	chcols(pc),d2
	bcs	.hi2
	move	chcols(pc),d2
	subq	#1,d2
.hi2	move	d2,cursx
	move	d3,cursy
	bsr	showhprop
	bsr	showvprop
	movem	(a7)+,d2-d3/d5
	move.l	(a7)+,cursx
	rts

sortblox	move	btopx(pc),d0
	move	btopy(pc),d1
	move	bbotx(pc),d2
	move	bboty(pc),d3
	cmp	d3,d1
	bcs	.ok
	beq	.skip
.swap	exg	d0,d2
	exg	d1,d3
	move	d0,btopx
	move	d1,btopy
	move	d2,bbotx
	move	d3,bboty
	rts
.skip	cmp	d2,d0
	bgt	.swap
.ok	rts

orderem	cmp	d3,d1
	bcs	.noswap
	bne	.swap
	cmp	d2,d0
	ble	.noswap	
.swap	exg	d0,d2
	exg	d1,d3
.noswap	rts

invertarea	;invert d0,d1 to d2,d3 (chars)
	;
	bsr	orderem
invertarea2	move.l	bitmaps(pc),a0
	move.l	bitmaps+4(pc),a1
	move	d1,d4
	mulu	fontmul(pc),d4
	add.l	d4,a0
	add.l	d4,a1
	sub	cursoff(pc),d2
	sub	cursoff(pc),d0
	;
.more2	bmi	.nextchar
	cmp	edcols(pc),d0
	bcc	.nextrow
	move	fonthite2(pc),d4
.loop	not.b	0(a0,d0)
	not.b	0(a1,d0)
	add	bmapmul(pc),a0
	add	bmapmul(pc),a1
	dbf	d4,.loop
	sub	fontmul(pc),a0
	sub	fontmul(pc),a1
	;
.nextchar	cmp	d2,d0
	blt	.more
	cmp	d3,d1
	bcs	.more
.done	rts
.more	addq	#1,d0
	bra	.more2
.nextrow	cmp	d3,d1
	bcc	.done
	move	cursoff(pc),d0
	neg	d0
	subq	#1,d0
	addq	#1,d1
	add	fontmul(pc),a0
	add	fontmul(pc),a1
	bra	.nextchar

getmousexy	;get mouse coords in d0,d1
	;d2,d3=col,row
	;return mi if to hi or lo
	;
	move.l	myscreen(pc),a0
	move	16(a0),d1
	move	topskip(pc),d2
	cmp	d2,d1
	bcs	.done
	add	pixrows(pc),d2
	cmp	d2,d1
	bcc	.done
	move	18(a0),d0
	move	d0,d2
	moveq	#0,d3
	move	d1,d3
	sub	topskip(pc),d3
	lsr	#3,d2
	divu	fonthite(pc),d3
	rts
.done	move	18(a0),d0
	move	d0,d2
	lsr	#3,d2
	moveq	#-1,d3
	rts

allocfirst	bsr	afirst2
	bra	cls
afirst2	bsr	allocnull
	beq	.done
	move.l	a0,firstitem
	move.l	a0,enditem
	move.l	a0,thisitem
.done	rts

showall	;
	bsr	storepos
	bsr	cls
	move.l	thisitem(pc),a2
	move	cursy(pc),d1
.loop	beq	.skip
	move.l	lastitem(a2),a2
	subq	#1,d1
	bra	.loop
.skip	move	d1,cursy
	clr.l	scloc
	clr	chloc
	move	edrows(pc),d7
	subq	#1,d7
.loop2	move.l	nextitem(a2),-(a7)
	bsr	showa2item
	move.l	(a7)+,d0
	beq	restorepos
	move.l	d0,a2 
	bsr	godown
	dbf	d7,.loop2
	bra	restorepos

showtopitem	move.l	thisitem(pc),a2
	bsr	storepos
	clr.l	scloc
	clr	chloc
	move	cursy(pc),d1
.loop	beq	.skip
	move.l	lastitem(a2),d0
	beq	.skip2
	move.l	d0,a2
	subq	#1,d1
	bra	.loop
.skip	move	d1,cursy
	bsr	showa2item
.skip2	bra	restorepos

showbotitem	move.l	thisitem(pc),a2
	bsr	storepos
.loop	move	cursy(pc),d0
	addq	#1,d0
	cmp	edrows(pc),d0
	bcc	.skip
	move.l	nextitem(a2),d0
	beq	.skip2
	move.l	d0,a2
	bsr	godown
	bra	.loop
.skip	bsr	showa2item
.skip2	bra	restorepos

showthisitem	
	move.l	thisitem(pc),a2
showa2item	lea	chars(a2),a2
	move.l	chmap(pc),a3
	add	chloc(pc),a3
	move	cursx(pc),-(a7)
	moveq	#0,d3
	moveq	#0,d2
	;
	move.b	(a2)+,d0
	beq	endofline
	;
	cmp.b	label(pc),d0
	bne	.skip
	bsr	normprint
	;
.loop	addq	#1,d2
	move.b	(a2)+,d0
	beq	endofline
.skip	bsr	tokprint
	bra	.loop
	;
endofline	cmp	#-1,bboty
	beq	.done2
	move	curstop(pc),d0
	add	cursy(pc),d0
	cmp	btopy(pc),d0
	bcs	.done2
	cmp	bboty(pc),d0
	bhi	.done2
.done3	cmp	edcols(pc),d3
	bcc	.done2
	moveq	#32,d0
	move	d3,cursx
	bsr	print
	addq	#1,d3
	bra	.done3
.done2	move	(a7)+,cursx
	rts

normprint	move.b	d0,0(a3,d2)
normprint2	cmp	cursoff(pc),d2
	bcs	.skip
	cmp	edcols(pc),d3
	bcc	.skip
	move	d3,cursx
	bsr	print
	addq	#1,d3
.skip	rts

tokprint	tst	tokenise
	beq	normprint
	tst.b	d0
	bpl	normprint
	lsl	#8,d0
	move.b	(a2)+,d0
	exg	a3,a5
	bsr	findtoke
	exg.l	a3,a5
.doit	move	tokcol(pc),color
.doit2	move.b	(a5)+,d0
	beq	.pdone
	move.b	d0,0(a3,d2)
	bset.b	#7,0(a3,d2)
	bsr	normprint2
	addq	#1,d2
	bra	.doit2
.pdone	move	normcol(pc),color
	subq	#1,d2
	rts

charstobuff	;transfer on screen chars to character buffer (charrbuff)
	;
	;null terminate and put length into d5
	;
	move.l	charbuff(pc),a1
	;move chars from a2 to a1
	move.l	a1,-(a7)
	move	chcols(pc),d1
	subq	#1,d1
	move	tokenise(pc),d0
	bne	tokit
	move.l	a1,a0
.loop	move.b	(a2)+,(a0)+
	cmp.b	#32,-1(a0)
	beq	.skip
	move.l	a0,a1
.skip	dbf	d1,.loop
enddo	clr.b	(a1)+
	move.l	a1,d5
	sub.l	(a7)+,d5
	rts

tokit2	move.l	a1,-(a7)
	bra	tokit3

tokit	;tokenise the line!
	;
	;first, find last character in line!
	;
tokit4	cmp.b	#32,0(a2,d1)
	bne	tokskip
tokit3	subq	#1,d1
	bpl	tokit4
	addq	#4,a7
	clr.b	(a1)
	moveq	#1,d5
	rts
tokskip	move.l	a4,-(a7)
	;O.K. d1=last char on line
	moveq	#0,d0
	move	d0,invflag
	;
	;the main tokenise loop...
	;
.loop2	move.b	(a2)+,d2
	bclr	#7,d2
	cmp.b	quote(pc),d2
	bne	.hendrix
.quoteon	not	invflag
.pch	bsr	putchar
	bra	.loop2
	;
.hendrix	move	invflag(pc),d4
	bne	.pch
	bsr	alpha
	beq	.tsttoke
	;
.ppp2	cmp.b	#'_',d2
	beq	.unto2
	cmp.b	#'.',d2
	beq	.unto2
	cmp.b	#'\',d2
	beq	.unto2
	cmp.b	remark(pc),d2
	bne	.notrem
.remloop	bsr	putchar
	move.b	(a2)+,d2
	bclr	#7,d2
	bra	.remloop
.notrem	cmp.b	#'$',d2
	bne	.nothex
.hexloop	bsr	putchar
	move.b	(a2)+,d2
	bclr	#7,d2
	;
	cmp.b	#'0',d2
	bcs	.nothex
	cmp.b	#'f',d2
	bhi	.nothex
	cmp.b	#'9',d2
	bls	.hexloop
	cmp.b	#'A',d2
	bcs	.nothex
	cmp.b	#'a',d2
	bcc	.hexloop
	cmp.b	#'F',d2
	bls	.hexloop
	;
.nothex	bsr	putchar
	move.b	(a2)+,d2
	bclr	#7,d2
	cmp.b	quote(pc),d2
	beq	.quoteon
	bsr	alpha
	bne	.ppp2
	;
.tsttoke	subq	#1,a2
	move.l	tokens(pc),a3
.loop3	move.l	a2,a4
	move.l	a3,-(a7)
	addq	#6,a3
.loop4	move.b	(a3)+,d3
	beq	.found
	move.b	(a4)+,d2
	bclr	#7,d2
	or.b	#32,d3
	or.b	#32,d2
	cmp.b	d3,d2
	beq	.loop4
	;
.next	move.l	(a7)+,a3
	move.l	(a3),d3
	beq	.notoke
	move.l	d3,a3
	bra	.loop3
	;
.notoke	move.b	(a2)+,d2
	bclr	#7,d2
.unto2	bsr	putchar
	move.b	(a2)+,d2
	bclr	#7,d2
	cmp.b	quote(pc),d2
	beq	.quoteon
	bsr	alpha
	beq	.unto2
	bra	.ppp2

.found	move.b	(a4),d2
	bclr	#7,d2
	bsr	alpha
	beq	.next
	cmp.b	#'_',d2
	beq	.next
	move.l	(a7)+,a3
	addq	#4,a3
.putina3	move.b	(a3)+,(a1)
	bset	#7,(a1)+
	move.b	(a3),(a1)+
	move.l	a4,d4
	sub.l	a2,d4
	add	d4,d0
	cmp	d1,d0
	bhi	pchover2
	move.l	a4,a2
	bra	.loop2

putchar	move.b	d2,(a1)+
	addq	#1,d0
	cmp	d1,d0
	bhi	pchover
	rts
pchover	addq	#4,a7
pchover2	move.l	(a7)+,a4
	bra	enddo

alpha	;return eq if d2 is alphabetic
	cmp.b	#65,d2
	bcs	.done
	cmp.b	#122,d2
	bhi	.done
	cmp.b	#91,d2
	bcs	.yes
	cmp.b	#97,d2
	bcs	.done
.yes	cmp	d2,d2
.done	rts

alphad0	exg	d0,d2
	bsr	alpha
	exg	d0,d2
	rts

scrupall	moveq	#0,d7
scrollup	;screen screen up from d7 to bottom
	;
	move	edrows(pc),d6
	sub	d7,d6
	subq	#1,d6	;d6=rows to scroll
	bsr	getblitter
	move	#$9f0,$040(a5)
	tst	d6
	beq	.skip
	move	d7,d0
	move	chcols(pc),d1
	mulu	d1,d0
	move.l	chmap(pc),a0
	add.l	d0,a0
	move.l	a0,$054(a5)
	add	d1,a0
	move.l	a0,$050(a5)
	clr.l	$064(a5)
	move	d6,d0
	lsl	#6,d0
	lsr	#1,d1
	and	#63,d1
	or	d1,d0
	move	d0,$058(a5)
	jsr	waitblit(a6)
.skip	move	d7,d5
	mulu	fontmul(pc),d5
	move	blitmod(pc),d0
	move	d0,$064(a5)
	move	d0,$066(a5)
	move	d6,d4
	move	fonthite(pc),d2
	mulu	d2,d4
	lsl	#6,d4
	move	edcols(pc),d0
	lsr	#1,d0
	or	d0,d4
	lsl	#6,d2
	or	d0,d2
	move	edrows(pc),d3
	subq	#1,d3
	mulu	fontmul(pc),d3
	move.l	bitmaps(pc),a0
	bsr	scup2
	move.l	bitmaps+4(pc),a0
	move	#$9f0,$040(a5)
	bsr	scup2
	move	edrows(pc),d0
	subq	#1,d0
	mulu	chcols(pc),d0
	move.l	chmap(pc),a0
	add	d0,a0
	move.l	a0,$054(a5)
	move	#$2020,$074(a5)
	move	chcols(pc),d0
	lsr	#1,d0
	or	#64,d0
	move	d0,$058(a5)
	;
	bra	putblitter
	;
scup2	move.l	a0,a1
	cmp	#64,d4
	bcs	.skip0
	add.l	d5,a0
	move.l	a0,$054(a5)
	add	fontmul(pc),a0
	move.l	a0,$050(a5)
	move	d4,$058(a5)
	jsr	waitblit(a6)
.skip0	add.l	d3,a1
	move.l	a1,$054(a5)
	clr	$074(a5)
	cmp	#-1,bboty
	beq	.skip
	move	curstop(pc),d0
	add	edrows(pc),d0
	subq	#1,d0
	cmp	btopy(pc),d0
	bcs	.skip
	cmp	bboty(pc),d0
	bhi	.skip
	move	#-1,$074(a5)
.skip	move	#$1f0,$040(a5)
	move	d2,$058(a5)
	jmp	waitblit(a6)

scrdownall	moveq	#0,d7
scrolldown	;scroll screen down from d7 to bottom
	;
	bsr	getblitter
	move	edrows(pc),d6
	sub	d7,d6
	subq	#1,d6	;d6=rows to scroll
	beq	eraseline
	move	#$9f0,$040(a5)
	move	#2,$042(a5)
	move	chcols(pc),d1
	move	d1,d0
	mulu	edrows(pc),d0	
	move.l	chmap(pc),a0
	add.l	d0,a0
	subq	#2,a0
	move.l	a0,$054(a5)
	sub	d1,a0
	move.l	a0,$050(a5)
	clr.l	$064(a5)
	move	d6,d0
	lsl	#6,d0
	lsr	#1,d1
	or	d1,d0
	move	d0,$058(a5)
	jsr	waitblit(a6)
	move	blitmod(pc),d0
	move	d0,$064(a5)
	move	d0,$066(a5)
	move	edcols(pc),d0
	lsr	#1,d0
	mulu	fonthite(pc),d6
	lsl	#6,d6
	or	d0,d6	;bltsize
	;
	move.l	pixrows22(pc),d5
	moveq	#0,d0
	move	blitmod(pc),d0
	addq	#2,d0
	sub.l	d0,d5
	;
	move.l	bitmaps(pc),a0
	bsr	scdown2
	move.l	bitmaps+4(pc),a0
	bsr	scdown2
	;
eraseline	move	blitmod(pc),d0
	move	d0,$064(a5)
	move	d0,$066(a5)
	move.l	#$1f00000,$040(a5)
	move	d7,d6
	mulu	fontmul(pc),d6	;add
	move	fonthite(pc),d5
	lsl	#6,d5
	move	edcols(pc),d0
	lsr	#1,d0
	or	d0,d5		;bltsize
	clr	$074(a5)
	cmp	#-1,bboty
	beq	.skip0
	move	d7,d0
	add	curstop(pc),d0
	cmp	btopy(pc),d0
	bcs	.skip0
	cmp	bboty(pc),d0
	bhi	.skip0
	move	#-1,$074(a5)
.skip0	move.l	bitmaps(pc),a0
	add.l	d6,a0
	move.l	a0,$054(a5)
	move	d5,$058(a5)
	jsr	waitblit(a6)
	move.l	bitmaps+4(pc),a0
	add.l	d6,a0
	move.l	a0,$054(a5)
	move	d5,$058(a5)
	jsr	waitblit(a6)
	move	#$2020,$074(a5)
	mulu	chcols(pc),d7
	move.l	chmap(pc),a0
	add.l	d7,a0
	move.l	a0,$054(a5)
	move	chcols(pc),d0
	lsr	#1,d0
	and	#63,d0
	or	#64,d0
	move	d0,$058(a5)
	;
	bra	putblitter
	;
scdown2	add.l	d5,a0
	move.l	a0,$054(a5)
	sub	fontmul(pc),a0
	move.l	a0,$050(a5)
	move	d6,$058(a5)
	jmp	waitblit(a6)

godown	addq	#1,cursy
	moveq	#0,d0
	move	fontmul(pc),d0
	add.l	d0,scloc
	move	chcols(pc),d0
	add	d0,chloc
	rts

goup	;move cursor up one line
	subq	#1,cursy
	moveq	#0,d0
	move	fontmul(pc),d0
	sub.l	d0,scloc
	move	chcols(pc),d0
	sub	d0,chloc
	rts

allocnull	moveq	#lenitem+1,d0
	moveq	#1,d1
	move.l	4,a6
	move	#-1,memalt
	jsr	allocmem(a6)
	tst.l	d0
	beq	.skip
	move.l	d0,a0
	clr.l	nextitem(a0)
	clr.l	lastitem(a0)
	move	#$100,numchars(a0)
.skip	rts

donew	;erase all
	;return z=1 (beq) if a problem
	;
	move	normcol(pc),color
	bsr	blokdis
	clr.l	cursx
	clr.l	scloc
	clr	chloc
	clr	curstop
	clr	cursoff
	clr	allalt
	;
	clr.b	filepath
	clr.b	filename
	;
	move	#1,numlines
	bsr	allocfirst
	bsr	showhprop
	bsr	showvprop
	moveq	#-1,d0
.done	rts

freethis	;free up line at this item
	move.l	thisitem(pc),a1
	move.b	label(pc),d0
	;
	move.l	a1,-(a7)
	lea	chars(a1),a1
.loop	cmp.b	#32,(a1)+
	beq	.loop
	cmp.b	-(a1),d0
	move.l	(a7)+,a1
	;
;	cmp.b	chars(a1),d0
	bne	.skip
	bsr	mforget2
.skip	moveq	#lenitem,d0
	add.b	numchars(a1),d0
	move.l	4,a6
	move	d0,memalt
	jmp	freemem(a6)

freeall	;free all items in editor
	;
	move.l	4,a6
	move.l	firstitem(pc),a2
	move	numlines(pc),d2
	subq	#1,d2
.loop	move.l	a2,a1
	moveq	#lenitem,d0
	add.b	numchars(a1),d0
	move.l	nextitem(a1),a2
	move	d0,memalt
	jsr	freemem(a6)	
	dbf	d2,.loop
	rts

scdata	ds.b	84

calcscreen	lea	scdata(pc),a0
	moveq	#84,d0
	moveq	#1,d1
	sub.l	a1,a1
	callint	getscreendata
	;
	and	#$fff0,scdata+12	;multiple of 8 for width!
	;
	move	#12,topskip
	move	#8,botskip
	;
	move	topskip(pc),newwindow+2
	;
	move	scdata+44+32(pc),viewmode
	;
	move	scdata+12(pc),d0
	move	d0,newscreen+4	;use width for screen!
	move	d0,newwindow+4	;and window!
	;
	lsr	#3,d0
	move	d0,totsccols
	sub	mousecols(pc),d0
	subq	#2,d0
	move	d0,edcols
	;
	moveq	#0,d0
	move	scdata+14(pc),d0
	sub	topskip(pc),d0
	sub	botskip(pc),d0
	divu	fonthite(pc),d0
	subq	#1,d0
	move	d0,edrows
	mulu	fonthite(pc),d0
	move	d0,pixrows
	add	fonthite(pc),d0
	add	botskip(pc),d0
	addq	#1,d0
	move	d0,newwindow+6
	add	topskip(pc),d0
	move	d0,newscreen+6	
	rts

screenopen2	bsr	screenopen
	bsr	showvprop
	bsr	showhprop
	bra	showall

blitmod	dc	0

screenopen	bsr	calcscreen
	;
	lea	newscreen(pc),a0
	callint	openscreen
	move.l	d0,myscreen
	beq	.sfail
	move.l	d0,winscreen
	move.l	d0,reqstruct+12
	move.l	d0,a0
	move.l	88(a0),a0	;bitmap
	;
	move	(a0),d1
	move	d1,bmapmul
	;
	move	d1,d0
	sub	edcols(pc),d0
	move	d0,blitmod
	;
	move	d1,d0
	mulu	pixrows(pc),d0
	move	d0,pixrows2
	move.l	d0,pixrows22
	;
	move	d1,d0
	mulu	fonthite(pc),d0
	move	d0,fontmul
	;
	move	d1,d0
	mulu	topskip(pc),d0
	move.l	8(a0),a1	;bitplane
	move.l	a1,combitmap
	add.l	d0,a1
	move.l	a1,bitmaps
	move.l	12(a0),a1
	add.l	d0,a1
	move.l	a1,bitmaps+4
	;
	move	pixrows(pc),d0
	add	botskip(pc),d0
	addq	#1,d0
	mulu	bmapmul(pc),d0
	add.l	d0,a1
	move.l	a1,statsloc
	;
	move.l	4,a6
	move	chcols(pc),d0
	mulu	edrows(pc),d0
	moveq	#2,d1
	move	d1,memalt
	jsr	allocmem(a6)
	move.l	d0,chmap
	beq	.cfail
	;
	clr.l	newgads+4
	move.l	#$a0000,newgads+8
	clr.l	newgads2+4
	move.l	#4,newgads2+8
	;
	move	#7,ngc3+10
	move	#7,ngc4+6
	move	#7,ngc4+10 
	move	#4,newgads2+10
	;
	move	edcols(pc),d0
	lsl	#3,d0
	move	d0,newgads+4
	move	d0,newgads2+8
	;
	move	pixrows(pc),d0
	move	d0,newgads+10
	move	d0,newgads2+6
	;
	move	edcols(pc),d0
	swap	d0
	clr	d0
	divu	chcols(pc),d0
	move	d0,prop2sinfo+6
	move	newgads+10(pc),d0
	subq	#1,d0
	move	d0,ngc1+10
	move	d0,ngc2+10
	move	d0,ngc2+6
	move	newgads+4(pc),newgadbord
	move	newgads+4(pc),nextngad
	addq	#3,newgads+4
	addq	#2,newgads+6
	subq	#4,newgads+10
	;
	move	newgads2+8(pc),d0
	sub	#1,d0
	move	d0,ngc3
	move	d0,ngc4
	move	d0,ngc4+4
	move	newgads2+6(pc),newgadbord2+2
	move	newgads2+6(pc),nextngad2+2
	addq	#3,newgads2+4
	addq	#2,newgads2+6
	subq	#6,newgads2+8
	;
	lea	newwindow(pc),a0
	callint	openwindow
	move.l	d0,mywindow
	beq	.wfail
	;
	move.l	d0,mywindow2
	move.l	d0,a0
	lea	menu1(pc),a1
	jsr	setmenustrip(a6)
	move.l	mywindow(pc),a0
	move.l	50(a0),a0
	move.l	a0,myrast
	lea	newgadbord(pc),a1
	moveq	#0,d0
	moveq	#0,d1
	jsr	drawborder(a6)
	move.l	myrast(pc),a0
	lea	newgadbord2(pc),a1
	moveq	#0,d0
	moveq	#0,d1
	jsr	drawborder(a6)
	bsr	colors
	move.l	statsloc(pc),a1
	lea	statstext(pc),a3
	bsr	pstats
	move	#-1,mousealt
	;
	move.l	4.w,a6
	sub.l	a1,a1
	jsr	findtask(a6)
	move.l	d0,a0
	move.l	184(a0),oldmywindow
	move.l	mywindow(pc),184(a0)
	bsr	pointeron
	move	#-1,screensopen
	bsr	makecop
	moveq	#0,d0
	rts
.wfail	bsr	freechmap
.cfail	move.l	myscreen(pc),a0
	jsr	closescreen(a6)
.sfail	moveq	#-1,d0
	rts

oldmywindow	dc.l	0
screensopen	dc	0

coplist	dc.l	0

cols	dc	$357,$469,$58b,$59d,$6af
	dc	$59d,$58c,$47a,$469,$357
	dc	$246
col2	dc	0

copok	dc	0	;non zero=don't make!

makecop	;make fancy copper list...
	move	copok(pc),d0
	beq.s	.doit
	rts
.doit	move	palit+12(pc),d0
	lsl	#8,d0
	move.b	palit+15(pc),d0
	lsl.b	#4,d0
	or.b	palit+17(pc),d0
	move	d0,col2
	;
	move.l	4,a6
	moveq	#12,d0
	move.l	#$10001,d1
	jsr	allocmem(a6)
	move.l	d0,coplist
	move.l	d0,d7
	;
	move.l	gr(pc),a6
	move.l	d0,a0
	moveq	#24,d0
	jsr	-594(a6)
	;
	moveq	#0,d6
	lea	cols(pc),a5
.loop	move.l	d7,a1
	move.l	d6,d0
	moveq	#0,d1
	jsr	-378(a6)
	move.l	d7,a1
	jsr	-366(a6)
	;
	move.l	d7,a1
	move.l	#$184,d0
	moveq	#0,d1
	move	(a5)+,d1
	jsr	-372(a6)
	move.l	d7,a1
	jsr	-366(a6)
	;
	addq	#1,d6
	cmp	#12,d6
	bcs.s	.loop
	;
	move.l	d7,a1
	move.l	#10000,d0
	move.l	#255,d1
	jsr	-378(a6)
	;
	move.l	myscreen(pc),a0
	move.l	d7,a1
	move.l	44+20(a0),(a1)
	move.l	a1,44+20(a0)
	;
	move.l	int(pc),a6
	jmp	remakedisplay(a6)

pstats	move.b	(a3)+,d0
	beq	.skip
	move.l	a1,a0
	bsr	qprint
	addq	#1,a1
	bra	pstats
.skip	rts

freechmap	move.l	chmap(pc),a1
	move	chcols(pc),d0
	mulu	edrows(pc),d0
	move.l	4,a6
	move	d0,memalt
	jmp	freemem(a6)

screenclose	clr	screensopen
	move.l	4.w,a6
	sub.l	a1,a1
	jsr	findtask(a6)
	move.l	d0,a0
	move.l	oldmywindow(pc),184(a0)
	;
	bsr	freechmap
	bsr	flushmw
	move.l	mywindow(pc),a0
	callint	clearpointer
	move.l	mywindow(pc),a0
	jsr	clearmenustrip(a6)
	move.l	mywindow(pc),a0
	jsr	closewindow(a6)
	move.l	myscreen(pc),a0
	jmp	closescreen(a6)

mwait	btst	#7,$bfe001
	bne	mwait
	rts

halfcolors	move.l	myscreen(pc),a5
	lea	44(a5),a5
	lea	palit(pc),a4
	moveq	#0,d7
	moveq	#3,d6
	move.l	gr(pc),a6
.loop	moveq	#0,d0
	move	d7,d0
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d2
	move	(a4)+,d1
	lsr	#1,d1
	move	(a4)+,d2
	lsr	#1,d2
	move	(a4)+,d3
	lsr	#1,d3
	move.l	a5,a0
	jsr	setrgb4(a6)
	add	#1,d7
	dbf	d6,.loop
	rts

colors	move.l	myscreen(pc),a5
	lea	44(a5),a5
	lea	palit(pc),a4
	moveq	#0,d7
	moveq	#6,d6
	move.l	gr(pc),a6
.loop	moveq	#0,d0
	move	d7,d0
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d2
	move	(a4)+,d1
	move	(a4)+,d2
	move	(a4)+,d3
	move.l	a5,a0
	jsr	setrgb4(a6)
	add	#1,d7
	cmp	#4,d7
	bne	.no
	lea	mpalit(pc),a4
	moveq	#17,d7
.no	dbf	d6,.loop
	rts

getblitter	callgr	ownblitter
	jsr	waitblit(a6)
	move.l	#$dff000,a5
	moveq	#-1,d0
	move.l	d0,$044(a5)
	moveq	#0,d0
	move	d0,$042(a5)
	rts

putblitter	callgr	waitblit
	jmp	disownblitter(a6)

storepos	move.l	cursx(pc),cursx2
	move.l	scloc(pc),scloc2
	move	chloc(pc),chloc2
	rts

restorepos	move.l	cursx2(pc),cursx
	move.l	scloc2(pc),scloc
	move	chloc2(pc),chloc
	rts

cls	bsr	getblitter
	move	#$1f0,$040(a5)
	move.l	chmap(pc),$054(a5)
	clr	$066(a5)
	move	#$2020,$074(a5)
	move	chcols(pc),d0
	lsr	#1,d0
	and	#63,d0
	move	edrows(pc),d1
	lsl	#6,d1
	or	d0,d1
	move	d1,$058(a5)
	jsr	waitblit(a6)
	clr	$074(a5)
	move.l	bitmaps(pc),a0
	bsr	cls2
	jsr	waitblit(a6)
	move.l	bitmaps+4(pc),a0
	bsr	cls2
	bra	putblitter
cls2	move.l	a0,$054(a5)
	move	edcols(pc),d0
	lsr	#1,d0
	move	pixrows(pc),d1
	lsl	#6,d1
	or	d0,d1
	move	blitmod(pc),d0
	move	d0,$066(a5)
	move	d1,$058(a5)
	rts

pixrows22	dc.l	0

scrollrite	;scroll the screen rite a word
	bsr	getblitter
	move.l	#$9f00002,$040(a5)
	;
	move.l	pixrows22(pc),d7
	moveq	#0,d6
	move	blitmod(pc),d6
	addq	#2,d6
	sub.l	d6,d7
	;	
	move.l	bitmaps(pc),a0
	add.l	d7,a0
	bsr	scrite2
	jsr	waitblit(a6)
	move.l	bitmaps+4(pc),a0
	add.l	d7,a0
	bsr	scrite2
	bsr	putblitter
	;
	bsr	storepos
	clr.l	cursx
	clr.l	scloc
	clr	chloc
	move.l	chmap(pc),a5
	add	cursoff(pc),a5
	move	edrows(pc),d2
	subq	#1,d2
.loop	move.b	(a5)+,d0
	bsr	print3
	move	#1,cursx
	move.b	(a5),d0
	bsr	print3
	clr	cursx
	moveq	#0,d0
	move	fontmul(pc),d0
	add.l	d0,scloc
	move	chcols(pc),d0
	add	d0,a5
	subq	#1,a5
	add	d0,chloc
	add	#1,cursy
	dbf	d2,.loop
	move	normcol(pc),color
	bra	restorepos
	;
scrite2	move.l	a0,$054(a5)
	subq	#2,a0
	move.l	a0,$050(a5)
	bra	hscmods

scrollleft	;scroll the screen a word left
	bsr	getblitter
	move	#$9f0,$040(a5)
	move.l	bitmaps(pc),a0
	bsr	scleft2
	jsr	waitblit(a6)
	move.l	bitmaps+4(pc),a0
	bsr	scleft2
	bsr	putblitter
	;
	bsr	storepos
	move	edcols(pc),d0
	subq	#2,d0
	move	d0,cursx
	clr	cursy
	clr.l	scloc
	clr	chloc
	move.l	chmap(pc),a5
	add	cursoff(pc),a5
	add	d0,a5
	move	edrows(pc),d2
	subq	#1,d2
.loop	move.b	(a5),d0
	bsr	print3
	add	#1,cursx
	move.b	1(a5),d0
	bsr	print3
	subq	#1,cursx
	moveq	#0,d0
	move	fontmul(pc),d0
	add.l	d0,scloc
	move	chcols(pc),d0
	add	d0,a5
	add	d0,chloc
	add	#1,cursy
	dbf	d2,.loop
	move	normcol(pc),color
	bra	restorepos
	;
scleft2	move.l	a0,$054(a5)
	addq	#2,a0
	move.l	a0,$050(a5)
	;
hscmods	move	blitmod(pc),d0
	addq	#2,d0
	move	d0,$064(a5)
	move	d0,$066(a5)
	move	edcols(pc),d0
	subq	#2,d0
	lsr	#1,d0
	move	pixrows(pc),d1
	lsl	#6,d1
	or	d0,d1
	move	d1,$058(a5)
	rts

print3	move	normcol(pc),color
	btst	#7,d0
	beq	.skip
	bclr	#7,d0
	move	tokcol(pc),color
.skip	lea	print4(pc),a0
	bra	printa0
print	;print d0 at cursx,cursy,in color (1,2,3)
	lea	print2(pc),a0
	;
printa0	;	
	movem.l	d2-d4/a2,-(a7)
	;
	cmp	#-1,bboty
	beq	.skip0
	move	curstop(pc),d2
	add	cursy(pc),d2
	cmp	btopy(pc),d2
	bcs	.skip0
	bne	.skip01
	move	cursoff(pc),d3
	add	cursx(pc),d3
	cmp	btopx(pc),d3
	blt	.skip0
	cmp	bboty(pc),d2
	bcs	.skipdo
	cmp	bbotx(pc),d3
	bgt	.skip0
.skipdo	move	#-1,inverse
	bra	prinv
.skip01	cmp	bboty(pc),d2				
	bhi	.skip0
	bne	.skipdo
	move	cursoff(pc),d2
	add	cursx(pc),d2
	cmp	bbotx(pc),d2
	ble	.skipdo
.skip0	clr	inverse
prinv	jsr	(a0)
	movem.l	(a7)+,d2-d4/a2
	rts

print2	;
	move.b	d0,d2
	move	color(pc),d1
	cmp	normcol(pc),d1
	beq	.skipz
	or	#128,d2
.skipz	move.l	chmap(pc),a0
	add	chloc(pc),a0
	add	cursoff(pc),a0
	add	cursx(pc),a0
	move.b	d2,(a0)
print4	bsr	calcphys
	bsr	fontcalc
	moveq	#-1,d1
	moveq	#-1,d2
	btst	#0,color+1
	bne	.skip
	moveq	#0,d1
.skip	btst	#1,color+1
	bne	.skip2
	moveq	#0,d2
.skip2	move	fonthite2(pc),d3
	move	inverse(pc),d4
.loop	move.b	(a2),d0
	and.b	d1,d0
	eor.b	d4,d0
	move.b	d0,(a0)
	move.b	(a2),d0
	and.b	d2,d0
	eor.b	d4,d0
	move.b	d0,(a1)
	;
	lea	16(a2),a2
	add	bmapmul(pc),a0
	add	bmapmul(pc),a1
	dbf	d3,.loop
	rts

qprint	;print d0 to a0, 1 bitplane only, quick
	bsr	fontcalc
	move	fonthite2(pc),d0
.loop	move.b	(a2),(a0)
	lea	16(a2),a2
	add	bmapmul(pc),a0
	dbf	d0,.loop
	rts

fontcalc	move	d0,d1
	and	#15,d0
	cmp	#6,fonthite
	beq	.font6
	lea	font8(pc),a2
	add	d0,a2
	and	#$f0,d1
	lsl	#3,d1
	add	d1,a2
	rts
.font6	lea	font6(pc),a2
	add	d0,a2
	and	#$f0,d1
	lsl	#1,d1
	move	d1,d0
	lsl	#1,d1
	add	d0,d1
	add	d1,a2
	rts

calcpos	;calculate pos of cursor in bitmaps
	;
	move	cursy(pc),d1
	mulu	fontmul(pc),d1
	move.l	d1,scloc
	move	cursy(pc),d1
	mulu	chcols(pc),d1
	move	d1,chloc
	rts

calcphys	;calculate physical coords of cursor
	move.l	bitmaps(pc),a0
	move.l	bitmaps+4(pc),a1
	add.l	scloc(pc),a0
	add.l	scloc(pc),a1
	add	cursx(pc),a0
	add	cursx(pc),a1
	rts

cursor
cursor2	bsr	calcphys
	move	fonthite2(pc),d0
.loop	not.b	(a0)
	not.b	(a1)
	add	bmapmul(pc),a0
	add	bmapmul(pc),a1
	dbf	d0,.loop
	rts

showrow	move.l	#$20202020,numtext
	move	#$2000,numtext+4
	move	curstop(pc),d0
	add	cursy(pc),d0
	addq	#1,d0
	lea	numtext(pc),a0
	bsr	numtoascii
	move.l	statsloc(pc),a1
	addq	#5,a1
	lea	numtext(pc),a3
	bra	pstats	

showvprop	bsr	showrow
	moveq	#-1,d4
	move	edrows(pc),d0
	cmp	numlines(pc),d0
	bcc	.skip
	swap	d0
	clr	d0
	divu	numlines(pc),d0
	move	d0,d4
.skip	moveq	#-1,d2
	move	curstop(pc),d0
	add	cursy(pc),d0
	move	numlines(pc),d1
	subq	#1,d1
	cmp	d1,d0
	bcc	.skip2
	swap	d0
	clr	d0
	divu	d1,d0
	move	d0,d2
.skip2	moveq	#-1,d1
	moveq	#-1,d3
	moveq	#1,d5
	moveq	#13,d0
	lea	newgads(pc),a0
	move.l	mywindow(pc),a1
	sub.l	a2,a2	
	move.l	int(pc),a6
	jmp	newmodifyprop(a6)

showhprop	move.l	#$20202000,numtext
	move	cursoff(pc),d0
	add	cursx(pc),d0
	addq	#1,d0
	lea	numtext(pc),a0
	bsr	numtoascii
	move.l	statsloc(pc),a1
	lea	18(a1),a1
	lea	numtext(pc),a3
	bsr	pstats	
	moveq	#-1,d1
	move	cursx(pc),d0
	add	cursoff(pc),d0
	move	chcols(pc),d2
	subq	#1,d2
	cmp	d2,d0
	bcc	.skip
	move	d0,d1
	swap	d1
	clr	d1
	divu	d2,d1
.skip	moveq	#-1,d2
	moveq	#-1,d4
	move	prop2sinfo+6(pc),d3
	moveq	#1,d5
	moveq	#11,d0
	lea	newgads2(pc),a0
	move.l	mywindow(pc),a1
	sub.l	a2,a2
	move.l	int(pc),a6
	jmp	newmodifyprop(a6)

refresh	;redraw entire screen using cursoff
	;
	bsr	storepos
	clr.l	scloc
	clr	chloc
	move	edrows(pc),d7
	subq	#1,d7
	clr	cursy
.loop	clr	cursx
	move.l	chmap(pc),a5
	add	chloc(pc),a5
	add	cursoff(pc),a5
	move	edcols(pc),d6
	subq	#1,d6
.loop2	move.b	(a5)+,d0
	bsr	print3
	add	#1,cursx
	dbf	d6,.loop2
	bsr	godown
	dbf	d7,.loop
	move	normcol(pc),color	
	bra	restorepos

inbloks	;return eq if cursor is <= start of first line of blok
	move	curstop(pc),d2
	add	cursy(pc),d2
	cmp	btopy(pc),d2
	bne	.skip
	move	cursx(pc),d2
	add	cursoff(pc),d2
	cmp	btopx(pc),d2
	bhi	.skip
	moveq	#0,d3	
.skip	rts

inblokf	;return eq if cursor is =< last char of blok
	move	curstop(pc),d2
	add	cursy(pc),d2
	cmp	bboty(pc),d2
	bne	.skip
	move	cursx(pc),d2
	add	cursoff(pc),d2
	cmp	bbotx(pc),d2
	bhi	.skip
	moveq	#0,d3
.skip	rts

blokfix	move	btopx(pc),d2
	cmp	chcols(pc),d2
	bcs	blokfixb
	clr	btopx
	add	#1,btopy
	move	btopy(pc),d2
	cmp	bboty(pc),d2
	bls	blokfixb
.skip	bra	blokdis
blokfixb	move	bbotx(pc),d2
	bpl	.ok2
	move	chcols(pc),d2
	subq	#1,d2
	move	d2,bbotx
	sub	#1,bboty
	move	bboty(pc),d2
	cmp	btopy(pc),d2
	bcc	.ok3
.zap	bra	blokdis
.ok2	move	btopy(pc),d2
	cmp	bboty(pc),d2
	bne	.ok4
	move	bbotx(pc),d2
	cmp	btopx(pc),d2
	blt	.zap
.ok4	cmp	chcols(pc),d2
	bcs	.ok3
	move	chcols(pc),d2
	subq	#1,d2
	move	d2,bbotx
.ok3	rts

getline	;get a line of input from user
	clr	anyalt
	bsr	cursor
	bra	getline3
getline2	bsr	cursor
	bsr	showhprop
getline3	bsr	getinput
	bsr	cursor
	;
	move.l	bitmaps(pc),a0
	move.l	bitmaps+4(pc),a1
	add.l	scloc(pc),a0
	add.l	scloc(pc),a1
	move.l	chmap(pc),a2
	add	chloc(pc),a2
	move	cursx(pc),d0
	;
	;***** HELP *****
	;
	tst.l	d7
	bmi	charhi
	bclr	#30,d7
	bne	.isasc
	cmp	#32,d7
	bcs	charlow
	cmp	#127,d7
	bcc	charhi
	;
.isasc	move	d7,anyalt
	;
	;adjust block
	;
	cmp	#-1,bboty
	beq	.skip0
	bsr	inbloks
	bne	.skipz
	add	#1,btopx
.skipz	bsr	inblokf
	bne	.skipy
.skipx	add	#1,bbotx
.skipy	bsr	blokfix
	;
.skip0	move	edcols(pc),d1
.loop	subq	#1,d1
	cmp	d1,d0
	bcc	.skip
	move	fonthite2(pc),d2
.loop2	move.b	-1(a0,d1),0(a0,d1)
	move.b	-1(a1,d1),0(a1,d1)
	add	bmapmul(pc),a0
	add	bmapmul(pc),a1
	dbf	d2,.loop2
	sub	fontmul(pc),a0
	sub	fontmul(pc),a1
	bra	.loop
.skip	move	chcols(pc),d1
	add	cursoff(pc),d0
.loop3	subq	#1,d1
	cmp	d1,d0
	bcc	.skip2
	move.b	-1(a2,d1),0(a2,d1)
	bra	.loop3
.skip2	move	d7,d0
	bsr	print
	move	cursx(pc),d0
	bsr	cursrite
	bra	getline2
	;
charlow	cmp	#31,d7	;curs left
	bne	charnot31	
	move	qualifier(pc),d1
	and	#3,d1
	beq	.skip
	;
	;shift cursor left.
	;
	move	d0,d1
	add	cursoff(pc),d1
	beq	.tababove
	moveq	#0,d1
	bra	curspos
	;
.tababove	;tab to line aboves tab pos.
	;
	move	curstop(pc),d1
	add	cursy(pc),d1
	beq	getline2
	move.l	a0,-(a7)
	move.l	thisitem(pc),a0
	move.l	4(a0),a0
	lea	chars(a0),a0
	moveq	#-1,d1
.loop	tst.b	(a0)
	beq	.none
	addq	#1,d1
	cmp.b	#32,(a0)+
	beq	.loop
	move.l	(a7)+,a0
	bra	curspos
.none	move.l	(a7)+,a0
	bra	getline2
	;
.skip	bsr	cursleft
	bra	getline2
	;
charnot31	cmp	#30,d7
	bne	charnot30
	move	qualifier(pc),d1
	and	#3,d1
	beq	cis30skip
	move	chcols(pc),d1
.loop	subq	#1,d1
	bpl	.skip
	moveq	#0,d1
	bra	curspos
.skip	cmp.b	#32,0(a2,d1)
	beq	.loop
	addq	#1,d1
	cmp	chcols(pc),d1
	bcs	curspos
	subq	#1,d1
curspos	;d1 holds column to go 4
	bsr	horizpos
	bra	getline2
cis30skip	bsr	cursrite
	bra	getline2
	;
charnot30	cmp	#8,d7
	bne	charnot8
	move	d0,d1
	or	cursoff(pc),d1
	beq	getline2
	movem.l	a0-a2,-(a7)
	bsr	cursleft
	movem.l	(a7)+,a0-a2
	move	cursx(pc),d0
	moveq	#8,d7
	bra	char127	
	;
charnot8	cmp	#9,d7
	bne	charnot9		
	add	cursoff(pc),d0
	move	d0,d2
	move	qualifier(pc),d1
	and	#3,d1
	beq	.loop
	;
	;delete tab
	;
	move	d7,anyalt
.loop4	subq	#1,d0
	bmi	getline2
	moveq	#0,d1
	move	d0,d1
	divu	tabstop(pc),d1
	swap	d1
	tst	d1
	bne	.loop4
	;
	;adjust block
	;
	cmp	#-1,bboty
	beq	.zip
	move	d2,-(a7)
	move	d2,d4
	sub	d0,d4
	bsr	inbloks
	bne	.zip1
	sub	d4,btopx
.zip1	bsr	inblokf
	bne	.zip2
	sub	d4,bbotx
	;
.zip2	move	(a7)+,d2
.zip	move	d0,-(a7)
.loop5	move.b	0(a2,d2),0(a2,d0)
	addq	#1,d2
	addq	#1,d0
	cmp	chcols(pc),d2
	bcs	.loop5
.loop6	move.b	#32,0(a2,d0)
	addq	#1,d0
	cmp	chcols(pc),d0
	bcs	.loop6
	bra	reline
	;
	;insert tab
	;
.loop	addq	#1,d0
	cmp	chcols(pc),d0
	bcc	getline2
	moveq	#0,d1
	move	d0,d1
	divu	tabstop(pc),d1
	swap	d1
	tst	d1
	bne	.loop
	move	d7,anyalt
	move	d0,-(a7)
	sub	d2,d0	;d0=number of chars to insert
	;
	;adjust for block
	;
	cmp	#-1,bboty
	beq	.zap
	move	d2,-(a7)
	bsr	inbloks
	bne	.zap2
	add	d0,btopx
.zap2	bsr	inblokf
	bne	.zap3
	add	d0,bbotx
.zap3	bsr	blokfix
	move	(a7)+,d2
	;
.zap	move	chcols(pc),d1
.loop2	subq	#1,d1
	move	d1,d3
	sub	d0,d3
	bmi	.done
	cmp	d2,d3
	bcs	.done
	move.b	0(a2,d3),0(a2,d1)
	bra	.loop2
.done	move.b	#32,0(a2,d1)
	subq	#1,d1
	bmi	reline
	cmp	d2,d1
	bcc	.done
reline	;redisplay line
	move	cursx(pc),-(a7)
	clr	cursx
	add	cursoff(pc),a2
	move	edcols(pc),d7
	subq	#1,d7
.loop3	move.b	(a2)+,d0
	bsr	print3
	add	#1,cursx
	dbf	d7,.loop3
	move	(a7)+,cursx
	move	(a7)+,d1
	bra	curspos
charnot9
	;
	bra	glinedone	
	
charhi
	cmp	#127,d7
	bne	charnot127
char127
	move	d7,anyalt
	;
	;adjust block
	;
	cmp	#-1,bboty
	beq	.loop
	bsr	inbloks
	bne	.skipz
	cmp	btopx(pc),d2
	beq	.skipz
	sub	#1,btopx
.skipz	bsr	inblokf
	bne	.loop
	move	bbotx(pc),d2
	addq	#1,d2
	cmp	chcols(pc),d2
	bcc	.skipmo
	sub	#1,bbotx
.skipmo	bsr	blokfixb
	;
.loop	addq	#1,d0
	cmp	edcols(pc),d0
	bcc	.skip
	move	fonthite2(pc),d1
.loop2	move.b	0(a0,d0),-1(a0,d0)
	move.b	0(a1,d0),-1(a1,d0)
	add	bmapmul(pc),a0
	add	bmapmul(pc),a1
	dbf	d1,.loop2
	sub	fontmul(pc),a0
	sub	fontmul(pc),a1
	bra	.loop	
.skip	move	cursx(pc),d0
	add	cursoff(pc),d0
.loop3	addq	#1,d0
	cmp	chcols(pc),d0
	bcc	.skip2
	move.b	0(a2,d0),-1(a2,d0)
	bra	.loop3
.skip2	move.b	#32,-1(a2,d0)
	move	cursx(pc),-(a7)
	move	edcols(pc),d0
	subq	#1,d0
	move	d0,cursx
	move	edcols(pc),d0
	add	cursoff(pc),d0
	subq	#1,d0
	move.b	0(a2,d0),d0
	bsr	print3
	move	(a7)+,cursx
	bra	getline2
	;
charnot127	cmp	#-1,d7
	bne	charnotgad
	cmp	#1,d6
	bne	charnotgad
	move	prop2sinfo+2(pc),d1
	mulu	chcols(pc),d1
	swap	d1
	bra	curspos
charnotgad	cmp	#-2,d7
	bne	charnotmenu
	cmp	#$f8e1,d6
	bne	charnotmenu
	move	d6,anyalt
	add	cursoff(pc),d0
	move	d0,-(a7)
.loop	move.b	#32,0(a2,d0)
	addq	#1,d0
	cmp	chcols(pc),d0
	bcs	.loop
	bra	reline
charnotmenu
	;
glinedone	rts

horizpos	;position cursor at d1
	;
	move	cursoff(pc),d4
	move	edcols(pc),d2
	sub	hscmarg(pc),d2
	cmp	d2,d1
	bcc	.skip3
	clr	cursoff
	move	d1,cursx
.moverite	cmp	cursoff(pc),d4
	beq	.done
	bra	refresh
.skip3	move	edcols(pc),d2
	lsr	#1,d2
	move	d1,d0
	sub	d2,d0
	move	chcols(pc),d3
	sub	edcols(pc),d3
	cmp	d3,d0
	bgt	.skip4
	btst	#0,d0
	beq	.skip5
	addq	#1,d0
	subq	#1,d2
.skip5	move	d0,cursoff
	move	d2,cursx
	bra	.moverite
.skip4	move	d3,cursoff
	sub	d3,d1
	move	d1,cursx	
	bra	.moverite
.done	rts

cursleft	cmp	hscmarg(pc),d0
	bcs	.skip
.skip3	sub	#1,cursx
	rts
.skip	move	cursoff(pc),d1
	bne	.skip2
	tst	d0
	bne	.skip3
	rts
.skip2	sub	#2,cursoff
	bsr	scrollrite
	add	#1,cursx
	rts	

cursrite	move	cursoff(pc),d1
	move	edcols(pc),d2
	add	d2,d1
	cmp	chcols(pc),d1
	bcs	.skip
	sub	#1,d2 
	cmp	d2,d0
	bcs	.skip3
	rts
.skip	cmp	hscmarg(pc),d0
	bcs	.skip3
	sub	hscmarg(pc),d2
	cmp	d2,d0
	bcc	.skip2
.skip3	add	#1,cursx
	rts
.skip2	add	#2,cursoff
	bsr	scrollleft
	sub	#1,cursx
	rts

flash	move	d0,-(a7)
	move	#-1,d0
.loop	move	d0,$dff180
	dbf	d0,.loop
	move	(a7)+,d0
	rts

;inputor0	;receive input or return a 0
;	;
;	move.l	4,a6
;	move.l	mywindow(pc),a0
;	move.l	86(a0),a0
;	jsr	getmsg(a6)
;	move.l	d0,d7
;	bne	ginp2
;	rts

pickwindow	move.l	mywindow(pc),a2
	move.l	req_window(pc),d0
	beq.s	.skip
	move.l	d0,a2
.skip	rts

getinput2:	;get input - or 0 if non available
	;
	move.l	4,a6
	bsr	pickwindow
	move.l	86(a2),a2
	move.l	a2,a0
	jsr	getmsg(a6)
	tst.l	d0
	bne	handled0
	moveq	#0,d7
	rts

getinput:	;receive user input into d7
	;
	;if d7=255, then gadget hit, gadget number in d6
	;
	;if d7=254, then menu hit, menu number in d6
	;
	move.l	4,a6
ginp1	bsr	pickwindow
 	move.l	86(a2),a2
.loop	move.l	a2,a0
	jsr	waitport(a6)
	move.l	a2,a0
	jsr	getmsg(a6)
	tst.l	d0
	beq	.loop
	;
	bsr	handled0
	beq	ginp1
	;
	rts

handled0	;handle message in d0 - return ne if message understood, else eq
	;
	move.l	d0,a1
	;
	move.l	20(a1),d1		;class
	;
	cmp.l	#$20,d1
	beq.s	.yesgad
	cmp.l	#$40,d1
	bne	.notgadget
	;
.yesgad	move.l	28(a1),a0		;gadget!
	;
.yesgad2	move	38(a0),d6
	moveq	#-1,d7
	;
	cmp.b	#4,17(a0)		;string gadget?
	bne.s	.notst
	cmp	#9,24(a1)		;tabbed?
	beq	.again		;yes, ignore
	;
.notst	btst	#2,15(a0)
	beq	.done
	cmp	#$20,d1
	beq	.done
	;
	jsr	replymsg(a6)
	bsr	closereq
	moveq	#-1,d0
	rts
	;
.notgadget	move	24(a1),d0	;code
	;
	cmp.l	#$2000,d1
	beq	.again	;verify
	;
	cmp.l	#$100,d1
	bne	.notmenu
	cmp	#-1,d0
	beq	.again
	move	d0,d6
	moveq	#-2,d7
	bra	.done
	;
.notmenu	cmp.l	#$8,d1
	bne	.notmouse
	moveq	#-3,d7	;mouse down
	cmp	#$68,d0
	beq	.done
	moveq	#-4,d7	;mouse up
	bra	.done
	;
.notmouse	cmp.l	#$400,d1
	bne	.notkey
	;
	;get ascii (0-$bf)
	;
	btst	#7,d0
	bne	.again		;key up!
	move	26(a1),d1		;qualifier
	move	d1,qualifier
	;
	bsr	rawed+32+4	;to binclude
	moveq	#0,d7
	move.b	d0,d7
	beq	.again
	;
	tst.l	req_window
	beq.s	.notareq
	;
	;ok, flash checking for [return] and [esc]
	;
	cmp	#13,d0
	bne.s	.notreturn
	;
	;return! - find first closereq gadget!
	;
	move.l	req_window(pc),a0
	lea	62(a0),a0
.retloop	move.l	(a0),d1
	beq	.again
	move.l	d1,a0
	btst	#2,15(a0)
	beq.s	.retloop
	cmp.b	#1,17(a0)
	bne.s	.retloop
.khere	bsr	refme
	move	#$40,d1
	bra	.yesgad2
	;
.notreturn	cmp	#27,d0	;escape?
	bne	.again
	;
.doesc	moveq	#0,d7	;last found...
	move.l	req_window(pc),a0
	lea	62(a0),a0
.escloop	move.l	(a0),d1
	beq.s	.escdone
	move.l	d1,a0
	btst	#2,15(a0)
	beq.s	.escloop
	cmp.b	#1,17(a0)
	bne.s	.escloop
	move.l	a0,d7
	bra.s	.escloop
.escdone	tst.l	d7
	beq	.again
	move.l	d7,a0
	bra.s	.khere
	;
.notareq	lea	germs(pc),a0
	moveq	#ngerms-1,d0
.gloop	cmp.b	(a0)+,d7
	beq	.gskip
	addq	#1,a0
	dbf	d0,.gloop
	bra	.done
.gskip	move.b	(a0)+,d7
	bset	#30,d7
	bra	.done
	;
.notkey	cmp.l	#$200,d1
	bne.s	.again
	;
	;close gadget! - do an escape type thingy.
	;
	moveq	#0,d0
	bra.s	.doesc
	;
.done	jsr	replymsg(a6)
	moveq	#-1,d0
	rts
.again	jsr	replymsg(a6)
	moveq	#0,d7
	moveq	#0,d0
	rts

refme	;a0=gadget to blink on!...d0.b=pl if select else un-select
	;
	movem.l	d0-d1/a0-a2/a6,-(a7)
	or	#$80,12(a0)
	move.l	req_window(pc),a1
	sub.l	a2,a2
	moveq	#1,d0
	callint	refreshglist
	moveq	#4,d1
	calldos	delay
	movem.l	(a7),d0-d1/a0-a2/a6
	and	#$ff7f,12(a0)
	move.l	req_window(pc),a1
	sub.l	a2,a2
	moveq	#1,d0
	callint	refreshglist
	movem.l	(a7)+,d0-d1/a0-a2/a6
	rts

germs	dc.b	228,26,246,27,252,28
	dc.b	196,29,214,30,220,31
	
;-----------data below----------------------------;

mydata	
	dc.l	screenclose
	dc.l	screenopen2
comdata	;		;stuff passed to extern prog
	dc.l	openreq
	dc.l	closereq
	dc.l	getinput
	dc.l	error
	dc.l	detokallz
	dc.l	tokall
	dc.l	ezfreq
	dc.l	refgads
	dc.l	getinput2
combitmap	dc.l	0
myscreen	dc.l	0
extint	dc.l	0
	dc.l	colors
	dc.l	halfcolors
	dc.l	tokone	;tokenise one line.
	dc.l	openio
	dc.l	closeio
	dc.l	actgad
	dc.l	pointeron
	dc.l	sleepon
mywindow2	dc.l	0
	dc.l	0
	dc.l	extalloc
bmapmul	dc	0
req_window	dc.l	0
	;
topskip	dc	0	;pixels taken by intuition
botskip	dc	0	;ditto
chcols	dc	128	;columns in character map.
totsccols	dc	80	;total on screen char columns
edrows	dc	0	;total on screen char rows
pixrows	dc	0	;editable screen pixels
pixrows2	dc	0	;above times fonthite
edcols	dc	0	;number of editable columns
mousecols	dc	0	;number of columns taken by mousables
bitmaps	dc.l	0,0	;front bitmap, back
cursx	dc	0	;screen x
cursy	dc	0	;screen y
scloc	dc.l	0	;left col offset
chloc	dc	0	;ditto for char map
cursx2	dc	0
cursy2	dc	0
scloc2	dc.l	0
chloc2	dc	0
cursoff	dc	0	;left offset for scroll
curstop	dc	0	;top of screen cursoff
color	dc	1
qualifier	dc	0	;qualifier of last raw key hit
chmap	dc.l	0	;pointer to character map. 
anyalt	dc	0	;flag for if line has been altered
firstitem	dc.l	0	;pointers to first
enditem	dc.l	0	;and last lines in prog.
thisitem	dc.l	0	;line cursor is on.
numlines	dc	0	;number of lines in text
bloktopx	dc	0	;
bloktopy	dc	0	;position of block.
blokbotx	dc	0	;
blokboty	dc	0	;
btopx	dc	0	;the REAL values of above
btopy	dc	0	;(For print etc)
bbotx	dc	0	;
bboty	dc	0	;YEAH!
invflag	dc	0
inverse	dc	0	;flag for invert
memstack	dc.l	0	;stack for out of memory problems
filehand	dc.l	0	;file handle "     "      "
memalt	dc	0	;flag for memory change
	;
defstart	;below are defaults
	;
fonthite	dc	8
fonthite2	dc	7	;above -1 for dbf
fontmul	dc	0
tokenise	dc	0
makeicons	dc	1
normcol	dc	1	;normal colour
labcol	dc	2	;label colour
tokcol	dc	3	;token colour
propcol	dc	0	;colour being fucked with hprop
hscmarg	dc	12	;margin for horiz scroll
vscmarg	dc	6	;margin for vert scroll
tabstop	dc	2
palit	dc	8,8,8
	dc	15,15,15	
	dc	0,0,0
	dc	15,13,0
	;
origscrows	dc	0
	;
defend	;end of defaults
	;
defname	dc.b	'l:blitzeditor.opts',0
	even
	;
mpalit	dc	15,15,0
	dc	15,0,0
	dc	0,0,0
	;
statsloc	dc.l	0
statstext	dc.b	'Line:      Column:    '
	dc.b	'Largest Mem (K):      ',0
	even
bltext	dc.b	'Block',0
	even
numtext	dc	0,0,0
defalt	dc	0
req	dc.l	0
allalt	dc	0
	dc	' :'
filepath	dc.b	0
	ds.b	191
filename	dc.b	0
	ds.b	63
filetext	dc.b	'Name of file to load',0
filetext2	dc.b	'Name to save file as',0
blokpath	dc.b	0
	ds.b	191
blokname	dc.b	0
	ds.b	63
bloktext	dc.b	'Name of block to load',0
bloktext2	dc.b	'Name to save block as',0
blokop	dc	0	;flag for a block operation

mywindow	dc.l	0
myrast	dc.l	0

charbuff	dc.l	0
charbuff2	dc.l	0
charbuff3	dc.l	0
charbuff4	dc.l	0
dosbuff	dc.l	0

nummice	dc.l	0
mice	dc.l	0
mousealt	dc	0

tokeerr	dc.b	'?????',0
	dc.b	'Token Unavailable',0
	even

jumptable	;.b=ascii,.b= vert prop fix flag,.l=routine
	;
	dc.b	28,1
	dc.l	cursup
	dc.b	29,1
	dc.l	cursdown
	dc.b	13,1
	dc.l	return
	dc.b	255,1
	dc.l	gadget
	dc.b	254,0
	dc.l	menu
	dc.b	253,1
	dc.l	mousedown
	dc.b	139,0
	dc.l	help
	dc.b	129,0
	dc.l	blockstart
	dc.b	130,0
	dc.l	blockend
	dc.b	131,0
	dc.l	blocksave
	dc.b	132,0
	dc.l	blokcopy2
	;
	dc.b	0
	even

menutable	dc.b	0,0
	dc.l	new
	dc.b	$02,1
	dc.l	top
	dc.b	$12,1
	dc.l	bottom
	dc.b	$90,0
	dc.l	escape
	dc.b	$51,1
	dc.l	insertline
	dc.b	$61,1
	dc.l	deleteline
	dc.b	$41,0
	dc.l	blokforget
	dc.b	$01,1
	dc.l	blokcopy
	dc.b	$11,1
	dc.l	blokkill
	dc.b	$21,0
	dc.l	bloksave
	dc.b	$31,0
	dc.l	blokload
	dc.b	$10,1
	dc.l	fileopen
	dc.b	$20,0
	dc.l	filesave
	dc.b	$30,0
	dc.l	defaults
	dc.b	$81,1
	dc.l	join
	dc.b	$91,1
	dc.l	blocktab
	dc.b	$a1,1
	dc.l	blockuntab
	dc.b	$b1,0
	dc.l	markindent
	dc.b	$03,1
	dc.l	find
	dc.b	$13,1
	dc.l	next
	dc.b	$23,1
	dc.l	previous
	dc.b	$33,1
	dc.l	replace
	dc.b	$43,1
	dc.l	replaceall
	dc.b	$50,0
	dc.l	prtfile
	dc.b	$60,0
	dc.l	mycli
	dc.b	$22,1
	dc.l	gotoline
	dc.b	$70,0
	dc.l	closewb
	dc.b	$80,0
	dc.l	openwb
	dc.b	$40,0
	dc.l	doabout
	dc.b	255
	even

error1	dc.b	'Couldn',39,'t open file!',0
	even
error2	dc.b	'Error reading file!',0
	even
error3	dc.b	'Error writing file!',0
	even

returncode	dc	0
external	dc.l	0
tokens	dc.l	0
menitems	dc.l	0
	;
loadxtra	dc.l	0
savextra	dc.l	0
	;
menurout	dc.l	0
cleanup	dc.l	0
colon	dc.b	255
quote	dc.b	255
label	dc.b	255
remark	dc.b	255
	dc.b	0
	even

;-----------intuition stuff below-----------------;

newscreen	dc	0,0,640,200,2
	dc.b	1,2
viewmode	dc	$8000+4,$100f
	dc.l	screenfont,newsctit,0,0
	;
	dc.l	extnewscreen

screenfont	dc.l	screenfontname
	dc	8
	dc.b	0,0

screenfontname
	dc.b	"topaz.font",0
	even

extnewscreen
	dc.l	1<<31+32+$1a
	dc.l	pens
 
	dc.l 	$80000047,1		;like wb simon was here
	dc.l 	$80000025,2		;depth=2

	dc.l	0

pens	dc	1,2,2,1,2,3,1,0,3,-1

newsctit	dc.b	'Blitz Basic 2 - Ted Version 1.1',0
	ds.b	80
	even

newwindow	dc	0,0,640,200
	dc.b	0,1
idcmp	dc.l	$400+$40+$100+$8+$20
	dc.l	$100+$800+$1000
	dc.l	newgads,0,0
winscreen	dc.l	0,0,0,0
	dc	15

newgadbord	dc	0,0
	dc.b	1,0,1,3
	dc.l	ngc1
	dc.l	nextngad
ngc1	dc	15,0,0,0,0,0
nextngad	dc	0,0
	dc.b	2,0,1,3
	dc.l	ngc2
	dc.l	0
ngc2	dc	15,1,15,0,1,0

newgadbord2	dc	0,0
	dc.b	1,0,1,3
	dc.l	ngc3
	dc.l	nextngad2
ngc3	dc	0,0,0,0,0,7
nextngad2	dc	0,0
	dc.b	2,0,1,3
	dc.l	ngc4
	dc.l	0
ngc4	dc	0,1,0,7,0,7

newgads	dc.l	newgads2
	dc	0,0,10,0
	dc	4,3,3
	dc.l	proprend,0
	dc.l	0,0,prop1sinfo
	dc	0
	dc.l	0

proprend	dc	0,0,0,0,0
	dc.l	0
	dc.b	0,0
	dc.l	0

prop1sinfo	dc	13,-1,0,-1,-1
	dc	0,0,0,0,0,0
		
newgads2	dc.l	0
	dc	0,0,0,4
	dc	4,1,3
	dc.l	proprend2,0
	dc.l	0,0,prop2sinfo
	dc	1
	dc.l	0

proprend2	dc	0,0,0,0,0
	dc.l	0
	dc.b	0,0
	dc.l	0

prop2sinfo	dc	11,0,0,$8000,-1
	dc	0,0,0,0,0,0

mp1	equ	2
mp2	equ	1
mj	equ	1

mwidth	equ	160
menu1	dc.l	menu4
	dc	0,0
	dc	64,10
	dc	1
	dc.l	menu1name	
	dc.l	menu1item1
	dc	0,0,0,0
menu1name	dc.b	'PROJECT',0
	even

menu1item1	dc.l	menu1item2
	dc	0,0
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m1i1name,0
	dc.b	'  '
	dc.l	0
	dc	0
	;
m1i1name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i1text
	dc.l	0
m1i1text	dc.b	'NEW',0
	even

menu1item2	dc.l	menu1item3
	dc	0,12
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m1i2name,0
	dc.b	'L '
	dc.l	0
	dc	0
m1i2name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i2text
	dc.l	0
m1i2text	dc.b	'LOAD',0
	even

menu1item3	dc.l	menu1item4
	dc	0,24
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m1i3name,0
	dc.b	'S '
	dc.l	0
	dc	0
m1i3name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i3text
	dc.l	0
m1i3text	dc.b	'SAVE',0
	even

menu1item4	dc.l	menu1item6
	dc	0,36
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m1i4name,0
	dc.b	'? '
	dc.l	0
	dc	0
m1i4name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i4text
	dc.l	0
m1i4text	dc.b	'DEFAULTS',0
	even

menu1item6	dc.l	menu1item7
	dc	0,48
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m1i6name,0
	dc.b	'  '
	dc.l	0
	dc	0
m1i6name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i6text
	dc.l	0
m1i6text	dc.b	'ABOUT',0
	even

menu1item7	dc.l	menu1item8
	dc	0,60
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m1i7name,0
	dc.b	'  '
	dc.l	0
	dc	0
m1i7name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i7text
	dc.l	0
m1i7text	dc.b	'PRINT',0
	even

menu1item8	dc.l	menu1item9
	dc	0,72
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m1i8name,0
	dc.b	'Z '
	dc.l	0
	dc	0
m1i8name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i8text
	dc.l	0
m1i8text	dc.b	'CLI',0
	even

menu1item9	dc.l	menu1item10
	dc	0,84
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m1i9name,0
	dc.b	'  '
	dc.l	0
	dc	0
m1i9name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i9text
	dc.l	0
m1i9text	dc.b	'CLOSE WORKBENCH',0
	even

menu1item10	dc.l	menu1item11
	dc	0,96
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m1i10name,0
	dc.b	'  '
	dc.l	0
	dc	0
m1i10name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i10text
	dc.l	0
m1i10text	dc.b	'OPEN WORKBENCH',0
	even

menu1item11	dc.l	0
	dc	0,108
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m1i11name,0
	dc.b	'Q '
	dc.l	0
	dc	0
m1i11name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m1i11text
	dc.l	0
m1i11text	dc.b	'QUIT',0
	even

menu2	dc.l	0
	dc	184,0
	dc	56,10
	dc	1
	dc.l	menu2name	
	dc.l	menu2item1
	dc	0,0,0,0
menu2name	dc.b	'SEARCH',0
	ds.b	8
	even

menu2item1	dc.l	menu2itema
	dc	0,0
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m2i1name,0
	dc.b	'F '
	dc.l	0
	dc	0
m2i1name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m2i1text
	dc.l	0
m2i1text	dc.b	'FIND',0
	ds.b	12
	even

menu2itema	dc.l	menu2item2
	dc	0,12
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m2ianame,0
	dc.b	'N '
	dc.l	0
	dc	0
m2ianame	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m2iatext
	dc.l	0
m2iatext	dc.b	'NEXT',0
	even

menu2item2	dc.l	menu2item3
	dc	0,24
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m2i2name,0
	dc.b	'P '
	dc.l	0
	dc	0
m2i2name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m2i2text
	dc.l	0
m2i2text	dc.b	'PREVIOUS',0
	even

menu2item3	dc.l	menu2item4
	dc	0,36
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m2i3name,0
	dc.b	'R '
	dc.l	0
	dc	0
m2i3name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m2i3text
	dc.l	0
m2i3text	dc.b	'REPLACE',0
	even

menu2item4	dc.l	0
	dc	0,48
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m2i4name,0
	dc.b	'  '
	dc.l	0
	dc	0
m2i4name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m2i4text
	dc.l	0
m2i4text	dc.b	'REPLACE ALL',0
	even

menu3	dc.l	menu2
	dc	120,0
	dc	56,10
	dc	1
	dc.l	menu3name	
	dc.l	menu3item1
	dc	0,0,0,0
menu3name	dc.b	'SOURCE',0
	even

menu3item1	dc.l	menu3item2
	dc	0,0
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m3i1name,0
	dc.b	'T '
	dc.l	0
	dc	0
m3i1name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m3i1text
	dc.l	0
m3i1text	dc.b	'TOP',0
	even

menu3item2	dc.l	menu3item3
	dc	0,12
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m3i2name,0
	dc.b	'B '
	dc.l	0
	dc	0
m3i2name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m3i2text
	dc.l	0
m3i2text	dc.b	'BOTTOM',0
	even

menu3item3	dc.l	0
	dc	0,24
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m3i3name,0
	dc.b	'G '
	dc.l	0
	dc	0
m3i3name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m3i3text
	dc.l	0
m3i3text	dc.b	'GOTO LINE',0
	even

menu4	dc.l	menu3
	dc	72,0
	dc	40,10
	dc	1
	dc.l	menu4name	
	dc.l	menu4item1
	dc	0,0,0,0
menu4name	dc.b	'EDIT',0
	even

menu4item1	dc.l	menu4item2
	dc	0,0
	dc	mwidth,12
	dc	2+64+4
	dc.l	0,m4i1name,0
	dc.b	'C '
	dc.l	0
	dc	0
m4i1name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i1text
	dc.l	0
m4i1text	dc.b	'COPY',0
	even

menu4item2	dc.l	menu4item4
	dc	0,12
	dc	mwidth,12
	dc	2+64+4
	dc.l	0,m4i2name,0
	dc.b	'K '
	dc.l	0
	dc	0
m4i2name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i2text
	dc.l	0
m4i2text	dc.b	'KILL',0
	even

menu4item4	dc.l	menu4item5
	dc	0,24
	dc	mwidth,12
	dc	2+64
	dc.l	0,m4i4name,0
	dc.b	'  '
	dc.l	0
	dc	0
m4i4name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i4text
	dc.l	0
m4i4text	dc.b	'BLOCK TO DISK',0
	even

menu4item5	dc.l	menu4item3
	dc	0,36
	dc	mwidth,12
	dc	2+16+64
	dc.l	0,m4i5name,0
	dc.b	'  '
	dc.l	0
	dc	0
m4i5name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i5text
	dc.l	0
m4i5text	dc.b	'INSERT FROM DISK',0
	even

menu4item3	dc.l	menu4item6
	dc	0,48
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4i3name,0
	dc.b	'W '
	dc.l	0
	dc	0
m4i3name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i3text
	dc.l	0
m4i3text	dc.b	'FORGET',0
	even

menu4item6	dc.l	menu4item7
	dc	0,60
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4i6name,0
	dc.b	'I '
	dc.l	0
	dc	0
m4i6name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i6text
	dc.l	0
m4i6text	dc.b	'INSERT LINE',0
	even

menu4item7	dc.l	menu4item8
	dc	0,72
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4i7name,0
	dc.b	'D '
	dc.l	0
	dc	0
m4i7name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i7text
	dc.l	0
m4i7text	dc.b	'DELETE LINE',0
	even

menu4item8	dc.l	menu4item9
	dc	0,84
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4i8name,0
	dc.b	'Y '
	dc.l	0
	dc	0
m4i8name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i8text
	dc.l	0
m4i8text	dc.b	'DELETE RIGHT',0
	even

menu4item9	dc.l	menu4itema
	dc	0,96
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4i9name,0
	dc.b	'J '
	dc.l	0
	dc	0
m4i9name	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4i9text
	dc.l	0
m4i9text	dc.b	'JOIN',0
	even

menu4itema	dc.l	menu4itemb
	dc	0,96+12
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4ianame,0
	dc.b	'] '
	dc.l	0
	dc	0
m4ianame	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4iatext
	dc.l	0
m4iatext	dc.b	'BLOCK TAB',0
	even

menu4itemb	dc.l	menu4itemc
	dc	0,96+24
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4ibname,0
	dc.b	'[ '
	dc.l	0
	dc	0
m4ibname	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4ibtext
	dc.l	0
m4ibtext	dc.b	'BLOCK UNTAB',0
	even

menu4itemc	dc.l	0
	dc	0,96+36
	dc	mwidth,12
	dc	2+16+64+4
	dc.l	0,m4icname,0
	dc.b	'A '
	dc.l	0
	dc	0
m4icname	dc.b	mp1,mp2,mj,0
	dc	2,2
	dc.l	0
	dc.l	m4ictext
	dc.l	0
m4ictext	dc.b	'MARK INDENT',0
	even

findreq	dc.l	0
	dc.w	80,6,376,92,0,0
	dc.l	findreqga1,findreqbo1,findreqin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

findreqbo1	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla1,findreqla2
findreqla1	dc.w	359,40,80,40,80,51
findreqla2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla3,findreqbo2
findreqla3	dc.w	359,41,359,51,81,51
findreqbo2	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla4,findreqla5
findreqla4	dc.w	359,24,80,24,80,35
findreqla5	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla6,findreqbo3
findreqla6	dc.w	359,25,359,35,81,35
findreqbo3	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla7,findreqla8
findreqla7	dc.w	367,20,8,20,8,55
findreqla8	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla9,findreqbo4
findreqla9	dc.w	367,21,367,55,9,55
findreqbo4	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla10,findreqla11
findreqla10	dc.w	375,0,0,0,0,91
findreqla11	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla12,0
findreqla12	dc.w	375,1,375,91,1,91
findreqbo5	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla18,findreqla19
findreqla18	dc.w	39,0,0,0,0,11
findreqla19	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla20,0
findreqla20	dc.w	39,1,39,11,1,11
findreqbo6	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla22,findreqla23
findreqla22	dc.w	71,0,0,0,0,11
findreqla23	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla24,0
findreqla24	dc.w	71,1,71,11,1,11
findreqbo7	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla26,findreqla27
findreqla26	dc.w	63,0,0,0,0,11
findreqla27	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla28,0
findreqla28	dc.w	63,1,63,11,1,11
findreqbo8	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla30,findreqla31
findreqla30	dc.w	95,0,0,0,0,11
findreqla31	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla32,0
findreqla32	dc.w	95,1,95,11,1,11
findreqbo9	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla34,findreqla35
findreqla34	dc.w	55,0,0,0,0,11
findreqla35	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla36,0
findreqla36	dc.w	55,1,55,11,1,11
findreqbo10	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	findreqla38,findreqla39
findreqla38	dc.w	119,0,0,0,0,11
findreqla39	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	findreqla40,0
findreqla40	dc.w	119,1,119,11,1,11

findreqin1	dc.b	1,0,1,0
	dc.w	12,42
	dc.l	0
	dc.l	findreqla13,findreqin2
findreqla13	dc.b	'REPLACE:',0
	even
findreqin2	dc.b	1,0,1,0
	dc.w	12,26
	dc.l	0
	dc.l	findreqla14,findreqin3
findreqla14	dc.b	'FIND:',0
	even
findreqin3	dc.b	1,0,1,0
	dc.w	12,6
	dc.l	0
	dc.l	findreqla15,0
findreqla15	dc.b	'Find/Replace',0
	even
findreqin4	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	findreqla21,0
findreqla21	dc.b	'NEXT',0
	even
findreqin5	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	findreqla25,0
findreqla25	dc.b	'PREVIOUS',0
	even
findreqin6	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	findreqla29,0
findreqla29	dc.b	'REPLACE',0
	even
findreqin7	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	findreqla33,0
findreqla33	dc.b	'REPLACE ALL',0
	even
findreqin8	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	findreqla37,0
findreqla37	dc.b	'CANCEL',0
	even
findreqin9	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	findreqla41,0
findreqla41	dc.b	'CASE SENSITIVE',0
	even

findreqga1	dc.l	findreqga2
	dc.w	84,26,272,8,$200,1,4
	dc.l	0,0,0,0,findreqsg1
	dc.w	1
	dc.l	0
findreqsg1	dc.l	findreqla16,undobuffer
	dc.w	0,64,0
	dc.w	0,0,0,0,0,0,0
	dc.l	0,0
findreqla16	dcb.b	64,0
	even
findreqga2	dc.l	findreqga3
	dc.w	84,42,272,8,$200,5,4
	dc.l	0,0,0,0,findreqsg2
	dc.w	2
	dc.l	0
findreqsg2	dc.l	findreqla17,undobuffer
	dc.w	0,64,0
	dc.w	0,0,0,0,0,0,0
	dc.l	0,0
findreqla17	dcb.b	64,0
	even
findreqga3	dc.l	findreqga4
	dc.w	8,76,40,12,0,5,1
	dc.l	findreqbo5,0,findreqin4,0,0
	dc.w	3
	dc.l	0
findreqga4	dc.l	findreqga5
	dc.w	56,76,72,12,0,5,1
	dc.l	findreqbo6,0,findreqin5,0,0
	dc.w	4
	dc.l	0
findreqga5	dc.l	findreqga6
	dc.w	136,76,64,12,0,5,1
	dc.l	findreqbo7,0,findreqin6,0,0
	dc.w	5
	dc.l	0
findreqga6	dc.l	findreqga7
	dc.w	208,76,96,12,0,5,1
	dc.l	findreqbo8,0,findreqin7,0,0
	dc.w	6
	dc.l	0
findreqga7	dc.l	findreqga8
	dc.w	312,76,56,12,0,5,1
	dc.l	findreqbo9,0,findreqin8,0,0
	dc.w	7
	dc.l	0
findreqga8	dc.l	0
	dc.w	136,60,120,12,0,$101,1
	dc.l	findreqbo10,0,findreqin9,0,0
	dc.w	8
	dc.l	0

undobuffer	ds.b	192

;rawmaker	incbin	rad:rawmaker.obj

rawed	incbin	rawed.obj

repall	dc.l	0
	dc.w	132,46,240,40,0,0
	dc.l	repallga1,repallbo1,repallin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

repallbo1	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	repallla1,repallla2
repallla1	dc.w	239,0,0,0,0,39
repallla2	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	repallla3,0
repallla3	dc.w	239,1,239,39,1,39
repallbo2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	repallla5,repallla6
repallla5	dc.w	79,0,0,0,0,11
repallla6	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	repallla7,0
repallla7	dc.w	79,1,79,11,1,11
repallbo3	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	repallla9,repallla10
repallla9	dc.w	71,0,0,0,0,11
repallla10	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	repallla11,0
repallla11	dc.w	71,1,71,11,1,11
repallbo4	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	repallla13,repallla14
repallla13	dc.w	55,0,0,0,0,11
repallla14	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	repallla15,0
repallla15	dc.w	55,1,55,11,1,11

repallin1	dc.b	1,0,1,0
	dc.w	12,6
	dc.l	0
	dc.l	repallla4,0
repallla4	dc.b	'Replace all.....',0
	even
repallin2	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	repallla8,0
repallla8	dc.b	'FROM HERE',0
	even
repallin3	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	repallla12,0
repallla12	dc.b	'FROM TOP',0
	even
repallin4	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	repallla16,0
repallla16	dc.b	'CANCEL',0
	even

repallga1	dc.l	repallga2
	dc.w	8,24,80,12,0,5,1
	dc.l	repallbo2,0,repallin2,0,0
	dc.w	1
	dc.l	0
repallga2	dc.l	repallga3
	dc.w	96,24,72,12,0,5,1
	dc.l	repallbo3,0,repallin3,0,0
	dc.w	2
	dc.l	0
repallga3	dc.l	0
	dc.w	176,24,56,12,0,5,1
	dc.l	repallbo4,0,repallin4,0,0
	dc.w	3
	dc.l	0

errorreq	dc.l	0
	dc.w	128,58,312,40,0,0
	dc.l	errorga1,errorbo1,errorin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

errorbo1	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	errorla1,errorla2
errorla1	dc.w	311,0,0,0,0,39
errorla2	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	errorla3,0
errorla3	dc.w	311,1,311,39,1,39
errorbo2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	errorla5,errorla6
errorla5	dc.w	55,0,0,0,0,11
errorla6	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	errorla7,0
errorla7	dc.w	55,1,55,11,1,11

errorin1	dc.b	1,0,1,0
	dc.w	12,6
	dc.l	0
errorpnt	dc.l	0,0
errorin2	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	errorla8,0
errorla8	dc.b	' OKAY ',0
	even

errorga1	dc.l	0
	dc.w	128,24,56,12,0,5,1
	dc.l	errorbo2,0,errorin2,0,0
	dc.w	1
	dc.l	0

erasereq	dc.l	0
	dc.w	128,50,256,48,0,0
	dc.l	erasereqga1,erasereqbo1,erasereqin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

erasereqbo1	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	erasereqla1,erasereqla2
erasereqla1	dc.w	255,0,0,0,0,47
erasereqla2	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	erasereqla3,0
erasereqla3	dc.w	255,1,255,47,1,47
erasereqbo2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	erasereqla6,erasereqla7
erasereqla6	dc.w	39,0,0,0,0,11
erasereqla7	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	erasereqla8,0
erasereqla8	dc.w	39,1,39,11,1,11
erasereqbo3	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	erasereqla10,erasereqla11
erasereqla10	dc.w	71,0,0,0,0,11
erasereqla11	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	erasereqla12,0
erasereqla12	dc.w	71,1,71,11,1,11

erasereqin1	dc.b	1,0,1,0
	dc.w	12,6
	dc.l	0
	dc.l	erasereqla4,erasereqin2
erasereqla4	dc.b	'This will erase altered file!',0
	even
erasereqin2	dc.b	1,0,1,0
	dc.w	12,18
	dc.l	0
	dc.l	erasereqla5,0
erasereqla5	dc.b	'Continue?',0
	even
erasereqin3	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	erasereqla9,0
erasereqla9	dc.b	' OK ',0
	even
erasereqin4	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	erasereqla13,0
erasereqla13	dc.b	' CANCEL ',0
	even

erasereqga1	dc.l	erasereqga2
	dc.w	8,32,40,12,0,5,1
	dc.l	erasereqbo2,0,erasereqin3,0,0
	dc.w	1
	dc.l	0
erasereqga2	dc.l	0
	dc.w	176,32,72,12,0,5,1
	dc.l	erasereqbo3,0,erasereqin4,0,0
	dc.w	2
	dc.l	0

defaultreq	dc.l	0
	dc.w	60,1,400,172,0,0
	dc.l	defaultreqga1,defaultreqbo1,defaultreqin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

defaultreqbo1	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla1,defaultreqla2
defaultreqla1	dc.w	47,52,16,52,16,63
defaultreqla2	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla3,defaultreqbo2
defaultreqla3	dc.w	47,53,47,63,17,63
defaultreqbo2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla4,defaultreqla5
defaultreqla4	dc.w	47,64,16,64,16,75
defaultreqla5	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla6,defaultreqbo3
defaultreqla6	dc.w	47,65,47,75,17,75
defaultreqbo3	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla7,defaultreqla8
defaultreqla7	dc.w	47,76,16,76,16,87
defaultreqla8	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla9,defaultreqbo4
defaultreqla9	dc.w	47,77,47,87,17,87
defaultreqbo4	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla10,defaultreqla11
defaultreqla10	dc.w	255,12,8,12,8,91
defaultreqla11	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla12,defaultreqbo5
defaultreqla12	dc.w	255,13,255,91,9,91
defaultreqbo5	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla13,defaultreqla14
defaultreqla13	dc.w	391,12,264,12,264,91
defaultreqla14	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla15,defaultreqbo6
defaultreqla15	dc.w	391,13,391,91,265,91
defaultreqbo6	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla16,defaultreqla17
defaultreqla16	dc.w	391,96,8,96,8,151
defaultreqla17	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla18,defaultreqbo7
defaultreqla18	dc.w	391,97,391,151,9,151
defaultreqbo7	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla19,defaultreqla20
defaultreqla19	dc.w	399,0,0,0,0,171
defaultreqla20	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla21,defaultreqbo8
defaultreqla21	dc.w	399,1,399,171,1,171
defaultreqbo8	dc.w	4,0
	dc.b	1,2,1,3
	dc.l	defaultreqla36,defaultreqla37
defaultreqla36	dc.w	379,118,68,118,68,125
defaultreqla37	dc.w	4,0
	dc.b	2,1,1,3
	dc.l	defaultreqla38,defaultreqbo9
defaultreqla38	dc.w	379,119,379,125,69,125
defaultreqbo9	dc.w	4,0
	dc.b	1,2,1,3
	dc.l	defaultreqla40,defaultreqla41
defaultreqla40	dc.w	379,130,68,130,68,137
defaultreqla41	dc.w	4,0
	dc.b	2,1,1,3
	dc.l	defaultreqla42,defaultreqbo10
defaultreqla42	dc.w	379,131,379,137,69,137
defaultreqbo10	dc.w	4,0
	dc.b	1,2,1,3
	dc.l	defaultreqla44,defaultreqla45
defaultreqla44	dc.w	379,142,68,142,68,149
defaultreqla45	dc.w	4,0
	dc.b	2,1,1,3
	dc.l	defaultreqla46,0
defaultreqla46	dc.w	379,143,379,149,69,149
defaultreqbo11	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla50,defaultreqla51
defaultreqla50	dc.w	31,0,0,0,0,11
defaultreqla51	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla52,0
defaultreqla52	dc.w	31,1,31,11,1,11
defaultreqbo12	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla54,defaultreqla55
defaultreqla54	dc.w	31,0,0,0,0,11
defaultreqla55	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla56,0
defaultreqla56	dc.w	31,1,31,11,1,11
defaultreqbo13	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla58,defaultreqla59
defaultreqla58	dc.w	31,0,0,0,0,11
defaultreqla59	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla60,0
defaultreqla60	dc.w	31,1,31,11,1,11
	;
	;
defaultreqbo14	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla62,defaultreqla63
defaultreqla62	dc.w	15,0,0,0,0,11
defaultreqla63	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla64,0
defaultreqla64	dc.w	15,1,15,11,1,11
defaultreqbo15	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla66,defaultreqla67
defaultreqla66	dc.w	15,0,0,0,0,11
defaultreqla67	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla68,0
defaultreqla68	dc.w	15,1,15,11,1,11
defaultreqbo16	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla70,defaultreqla71
defaultreqla70	dc.w	15,0,0,0,0,11
defaultreqla71	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla72,0
defaultreqla72	dc.w	15,1,15,11,1,11
defaultreqbo17	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla74,defaultreqla75
defaultreqla74	dc.w	15,0,0,0,0,11
defaultreqla75	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla76,0
defaultreqla76	dc.w	15,1,15,11,1,11
defaultreqbo18	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla78,defaultreqla79
defaultreqla78	dc.w	15,0,0,0,0,11
defaultreqla79	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla80,0
defaultreqla80	dc.w	15,1,15,11,1,11
defaultreqbo19	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla82,defaultreqla83
defaultreqla82	dc.w	15,0,0,0,0,11
defaultreqla83	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla84,0
defaultreqla84	dc.w	15,1,15,11,1,11
defaultreqbo20	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla86,defaultreqla87
defaultreqla86	dc.w	15,0,0,0,0,11
defaultreqla87	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla88,0
defaultreqla88	dc.w	15,1,15,11,1,11
defaultreqbo21	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla90,defaultreqla91
defaultreqla90	dc.w	15,0,0,0,0,11
defaultreqla91	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla92,0
defaultreqla92	dc.w	15,1,15,11,1,11
defaultreqbo22	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla94,defaultreqla95
defaultreqla94	dc.w	15,0,0,0,0,11
defaultreqla95	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla96,0
defaultreqla96	dc.w	15,1,15,11,1,11
defaultreqbo23	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla98,defaultreqla99
defaultreqla98	dc.w	15,0,0,0,0,11
defaultreqla99	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla100,0
defaultreqla100	dc.w	15,1,15,11,1,11
defaultreqbo24	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla102,defaultreqla103
defaultreqla102	dc.w	15,0,0,0,0,11
defaultreqla103	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla104,0
defaultreqla104	dc.w	15,1,15,11,1,11
defaultreqbo25	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla106,defaultreqla107
defaultreqla106	dc.w	15,0,0,0,0,11
defaultreqla107	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla108,0
defaultreqla108	dc.w	15,1,15,11,1,11
defaultreqbo26	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla110,defaultreqla111
defaultreqla110	dc.w	71,0,0,0,0,11
defaultreqla111	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla112,0
defaultreqla112	dc.w	71,1,71,11,1,11
defaultreqbo27	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla114,defaultreqla115
defaultreqla114	dc.w	71,0,0,0,0,11
defaultreqla115	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla116,0
defaultreqla116	dc.w	71,1,71,11,1,11
defaultreqbo28	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla118,defaultreqla119
defaultreqla118	dc.w	71,0,0,0,0,11
defaultreqla119	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla120,0
defaultreqla120	dc.w	71,1,71,11,1,11
defaultreqbo29	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla122,defaultreqla123
defaultreqla122	dc.w	71,0,0,0,0,11
defaultreqla123	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla124,0
defaultreqla124	dc.w	71,1,71,11,1,11
	;
	;top
	;bottom
	;
defaultreqbo30	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla126,defaultreqla127
defaultreqla126	dc.w	39,0,0,0,0,11
defaultreqla127	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla128,0
defaultreqla128	dc.w	39,1,39,11,1,11
defaultreqbo31	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	defaultreqla130,defaultreqla131
defaultreqla130	dc.w	71,0,0,0,0,11
defaultreqla131	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	defaultreqla132,0
defaultreqla132	dc.w	71,1,71,11,1,11

defaultreqin1	dc.b	1,0,1,0
	dc.w	20,2
	dc.l	0
	dc.l	defaultreqla22,defaultreqin2
defaultreqla22	dc.b	'Default Options',0
	even
defaultreqin2	dc.b	1,0,1,0
	dc.w	56,18
	dc.l	0
	dc.l	defaultreqla23,defaultreqin3
defaultreqla23	dc.b	'Use Small Font',0
	even
defaultreqin3	dc.b	1,0,1,0
	dc.w	56,30
	dc.l	0
	dc.l	defaultreqla24,defaultreqin4
defaultreqla24	dc.b	'Tokenise',0
	even
defaultreqin4	dc.b	1,0,1,0
	dc.w	56,42
	dc.l	0
	dc.l	defaultreqla25,defaultreqin5
defaultreqla25	dc.b	'Create Icons',0
	even
defaultreqin5	dc.b	1,0,1,0
	dc.w	56,54
	dc.l	0
	dc.l	defaultreqla26,defaultreqin6
defaultreqla26	dc.b	'Horizontal Scroll Margin',0
	even
defaultreqin6	dc.b	1,0,1,0
	dc.w	56,66
	dc.l	0
	dc.l	defaultreqla27,defaultreqin7
defaultreqla27	dc.b	'Vertical Scroll Margin',0
	even
defaultreqin7	dc.b	1,0,1,0
	dc.w	56,78
	dc.l	0
	dc.l	defaultreqla28,defaultreqin8
defaultreqla28	dc.b	'Tab Stop',0
	even
defaultreqin8	dc.b	1,0,1,0
	dc.w	276,18+8
	dc.l	0
	dc.l	defaultreqla29,defaultreqin9
defaultreqla29	dc.b	'Text Colour:',0
	even
defaultreqin9	dc.b	1,0,1,0
	dc.w	276,42
	dc.l	0
	dc.l	defaultreqla30,defaultreqin10
defaultreqla30	dc.b '',0 	;'Label Colour:',0
	even
defaultreqin10	dc.b	1,0,1,0
	dc.w	276,66-8
	dc.l	0
	dc.l	defaultreqla31,defaultreqin11
defaultreqla31	dc.b	'Token Colour:',0
	even
defaultreqin11	dc.b	1,0,1,0
	dc.w	20,118
	dc.l	0
	dc.l	defaultreqla32,defaultreqin12
defaultreqla32	dc.b	'Red:',0
	even
defaultreqin12	dc.b	1,0,1,0
	dc.w	20,130
	dc.l	0
	dc.l	defaultreqla33,defaultreqin13
defaultreqla33	dc.b	'Green:',0
	even
defaultreqin13	dc.b	1,0,1,0
	dc.w	20,142
	dc.l	0
	dc.l	defaultreqla34,0
defaultreqla34	dc.b	'Blue:',0
	even
defaultreqin14	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla53,0
defaultreqla53	dc.b	'  ',0
	even
defaultreqin15	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla57,0
defaultreqla57	dc.b	'  ',0
	even
defaultreqin16	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla61,0
defaultreqla61	dc.b	'  ',0
	even
defaultreqin17	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla65,0
defaultreqla65	dc.b	' ',0
	even
defaultreqin18	dc.b	1,1,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla69,0
defaultreqla69	dc.b	' ',0
	even
defaultreqin19	dc.b	1,2,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla73,0
defaultreqla73	dc.b	' ',0
	even
defaultreqin20	dc.b	1,3,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla77,0
defaultreqla77	dc.b	' ',0
	even
defaultreqin21	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla81,0
defaultreqla81	dc.b	' ',0
	even
defaultreqin22	dc.b	1,1,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla85,0
defaultreqla85	dc.b	' ',0
	even
defaultreqin23	dc.b	1,2,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla89,0
defaultreqla89	dc.b	' ',0
	even
defaultreqin24	dc.b	1,3,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla93,0
defaultreqla93	dc.b	' ',0
	even
defaultreqin25	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla97,0
defaultreqla97	dc.b	' ',0
	even
defaultreqin26	dc.b	1,1,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla101,0
defaultreqla101	dc.b	' ',0
	even
defaultreqin27	dc.b	1,2,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla105,0
defaultreqla105	dc.b	' ',0
	even
defaultreqin28	dc.b	1,3,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla109,0
defaultreqla109	dc.b	' ',0
	even
defaultreqin29	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla113,0
defaultreqla113	dc.b	'        ',0
	even
defaultreqin30	dc.b	1,1,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla117,0
defaultreqla117	dc.b	'        ',0
	even
defaultreqin31	dc.b	1,2,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla121,0
defaultreqla121	dc.b	'        ',0
	even
defaultreqin32	dc.b	1,3,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla125,0
defaultreqla125	dc.b	'        ',0
	even
defaultreqin33	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla129,0
defaultreqla129	dc.b	' OK ',0
	even
defaultreqin34	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	defaultreqla133,0
defaultreqla133	dc.b	' CANCEL ',0
	even


defaultreqga1	dc.l	defaultreqga2
	dc.w	75,120,306,4,0,3,3
	dc.l	defaultreqla35,0,0,0,defaultreqhp1
	dc.w	1
	dc.l	0
defaultreqla35	dc.l	0,0,0,0,0
defaultreqhp1	
	dc.w	11,(6<<16)/15,-1,4096,-1,0,0,0,0,0,0
defaultreqga2	dc.l	defaultreqga3
	dc.w	75,132,306,4,0,3,3
	dc.l	defaultreqla39,0,0,0,defaultreqhp2
	dc.w	2
	dc.l	0
defaultreqla39	dc.l	0,0,0,0,0
defaultreqhp2	
	dc.w	11,(6<<16)/15,-1,4096,-1,0,0,0,0,0,0
defaultreqga3	dc.l	defaultreqga4
	dc.w	75,144,306,4,0,3,3
	dc.l	defaultreqla43,0,0,0,defaultreqhp3
	dc.w	3
	dc.l	0
defaultreqla43	dc.l	0,0,0,0,0
defaultreqhp3	
	dc.w	11,(6<<16)/15,-1,4096,-1,0,0,0,0,0,0
defaultreqga4	dc.l	defaultreqga5
	dc.w	20,54,24,8,0,2049,4
	dc.l	0,0,0,0,defaultreqsg1
	dc.w	4
	dc.l	0
defaultreqsg1	dc.l	defaultreqla47,undobuffer
	dc.w	0,3,0
	dc.w	0,0,0,0,0,0,0
int1	dc.l	12,0
defaultreqla47	dc.b	'12'
	dcb.b	1,0
	even
defaultreqga5	dc.l	defaultreqga6
	dc.w	20,66,24,8,0,2049,4
	dc.l	0,0,0,0,defaultreqsg2
	dc.w	5
	dc.l	0
defaultreqsg2	dc.l	defaultreqla48,undobuffer
	dc.w	0,3,0
	dc.w	0,0,0,0,0,0,0
int2	dc.l	6,0
defaultreqla48	dc.b	'6'
	dcb.b	2,0
	even
defaultreqga6	dc.l	defaultreqga7
	dc.w	20,78,24,8,0,2049,4
	dc.l	0,0,0,0,defaultreqsg3
	dc.w	6
	dc.l	0
defaultreqsg3	dc.l	defaultreqla49,undobuffer
	dc.w	0,3,0
	dc.w	0,0,0,0,0,0,0
int3	dc.l	4,0
defaultreqla49	dc.b	'2'
	dcb.b	2,0
	even
	;
defaultreqga7	dc.l	defaultreqga8
	dc.w	16,16,32,12,0,257,1 ;$80,257,1
	dc.l	defaultreqbo11,0,defaultreqin14,0,0
	dc.w	7
	dc.l	0
defaultreqga8	dc.l	defaultreqga9
	dc.w	16,28,32,12,0,257,1
	dc.l	defaultreqbo12,0,defaultreqin15,0,0
	dc.w	8
	dc.l	0
defaultreqga9	dc.l	defaultreqga10
	dc.w	16,40,32,12,128,257,1
	dc.l	defaultreqbo13,0,defaultreqin16,0,0
	dc.w	9
	dc.l	0
defaultreqga10	dc.l	defaultreqga11
	dc.w	280,28+8,16,12,2,2,1
	dc.l	defaultreqbo14,defaultreqbo14,defaultreqin17,0,0
	dc.w	10
	dc.l	0
defaultreqga11	dc.l	defaultreqga12
	dc.w	304,28+8,16,12,2,2,1
	dc.l	defaultreqbo15,defaultreqbo15,defaultreqin18,0,0
	dc.w	11
	dc.l	0
defaultreqga12	dc.l	defaultreqga13
	dc.w	328,28+8,16,12,2,2,1
	dc.l	defaultreqbo16,defaultreqbo16,defaultreqin19,0,0
	dc.w	12
	dc.l	0
defaultreqga13	dc.l defaultreqga18	;dc.l	defaultreqga14
	dc.w	352,28+8,16,12,2,2,1
	dc.l	defaultreqbo17,defaultreqbo17,defaultreqin20,0,0
	dc.w	13
	dc.l	0
defaultreqga14	dc.l	defaultreqga15
	dc.w	280,52,16,12,2,2,1
	dc.l	defaultreqbo18,defaultreqbo18,defaultreqin21,0,0
	dc.w	14
	dc.l	0
defaultreqga15	dc.l	defaultreqga16
	dc.w	304,52,16,12,2,2,1
	dc.l	defaultreqbo19,defaultreqbo19,defaultreqin22,0,0
	dc.w	15
	dc.l	0
defaultreqga16	dc.l	defaultreqga17
	dc.w	328,52,16,12,2,2,1
	dc.l	defaultreqbo20,defaultreqbo20,defaultreqin23,0,0
	dc.w	16
	dc.l	0
defaultreqga17	dc.l	defaultreqga18
	dc.w	352,52,16,12,2,2,1
	dc.l	defaultreqbo21,defaultreqbo21,defaultreqin24,0,0
	dc.w	17
	dc.l	0
defaultreqga18	dc.l	defaultreqga19
	dc.w	280,76-8,16,12,2,2,1
	dc.l	defaultreqbo22,defaultreqbo22,defaultreqin25,0,0
	dc.w	18
	dc.l	0
defaultreqga19	dc.l	defaultreqga20
	dc.w	304,76-8,16,12,2,2,1
	dc.l	defaultreqbo23,defaultreqbo23,defaultreqin26,0,0
	dc.w	19
	dc.l	0
defaultreqga20	dc.l	defaultreqga21
	dc.w	328,76-8,16,12,2,2,1
	dc.l	defaultreqbo24,defaultreqbo24,defaultreqin27,0,0
	dc.w	20
	dc.l	0
defaultreqga21	dc.l	defaultreqga22
	dc.w	352,76-8,16,12,2,2,1
	dc.l	defaultreqbo25,defaultreqbo25,defaultreqin28,0,0
	dc.w	21
	dc.l	0
defaultreqga22	dc.l	defaultreqga23
	dc.w	16,100,72,12,2,2,1
	dc.l	defaultreqbo26,defaultreqbo26,defaultreqin29,0,0
	dc.w	22
	dc.l	0
defaultreqga23	dc.l	defaultreqga24
	dc.w	112,100,72,12,2,2,1
	dc.l	defaultreqbo27,defaultreqbo27,defaultreqin30,0,0
	dc.w	23
	dc.l	0
defaultreqga24	dc.l	defaultreqga25
	dc.w	216,100,72,12,2,2,1
	dc.l	defaultreqbo28,defaultreqbo28,defaultreqin31,0,0
	dc.w	24
	dc.l	0
defaultreqga25	dc.l	defaultreqga26
	dc.w	312,100,72,12,2,2,1
	dc.l	defaultreqbo29,defaultreqbo29,defaultreqin32,0,0
	dc.w	25
	dc.l	0
defaultreqga26	dc.l	defaultreqga27
	dc.w	8,156,40,12,0,5,1
	dc.l	defaultreqbo30,0,defaultreqin33,0,0
	dc.w	26
	dc.l	0
defaultreqga27	dc.l	0
	dc.w	320,156,72,12,0,5,1
	dc.l	defaultreqbo31,0,defaultreqin34,0,0
	dc.w	27
	dc.l	0

alertstuff	dc	56
	dc.b	22,'Not enough memory for operation!'
	dc.b	' - Click Mouse Button to Continue.',0,0
	even

flink	lea	filerequest+28(pc),a0
	move.l	(a0)+,d0
	move.l 	a0,a1
	move.l	a0,d2
	lsl.l	#2,d0
	lea	4(a0,d0.l),a0
	move.l	(a0),d0
	addq	#8,a0
.loop	move.l	(a0)+,d1
	add.l	d2,0(a1,d1.l)
	subq.l	#1,d0
	bne	.loop
	rts

filerequest	incbin	filereq

;	include	rad:filerequester

prtreq	dc.l	0
	dc.w	60,46,360,68,0,0
	dc.l	prtreqga1,prtreqbo1,prtreqin1
	dc.w	0
	dc.b	0,0
	dc.l	0
	ds.b	32
	dc.l	0,0
	ds.b	36

prtreqbo1	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	prtreqla1,prtreqla2
prtreqla1	dc.w	343,28,144,28,144,39
prtreqla2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	prtreqla3,prtreqbo2
prtreqla3	dc.w	343,29,343,39,145,39
prtreqbo2	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	prtreqla4,prtreqla5
prtreqla4	dc.w	351,24,8,24,8,43
prtreqla5	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	prtreqla6,prtreqbo3
prtreqla6	dc.w	351,25,351,43,9,43
prtreqbo3	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	prtreqla7,prtreqla8
prtreqla7	dc.w	359,0,0,0,0,67
prtreqla8	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	prtreqla9,0
prtreqla9	dc.w	359,1,359,67,1,67
prtreqbo4	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	prtreqla13,prtreqla14
prtreqla13	dc.w	63,0,0,0,0,11
prtreqla14	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	prtreqla15,0
prtreqla15	dc.w	63,1,63,11,1,11
prtreqbo5	dc.w	0,0
	dc.b	1,2,1,3
	dc.l	prtreqla17,prtreqla18
prtreqla17	dc.w	71,0,0,0,0,11
prtreqla18	dc.w	0,0
	dc.b	2,1,1,3
	dc.l	prtreqla19,0
prtreqla19	dc.w	71,1,71,11,1,11

prtreqin1	dc.b	1,0,1,0
	dc.w	20,30
	dc.l	0
	dc.l	prtreqla10,prtreqin2
prtreqla10	dc.b	'PRINTER DEVICE:',0
	even
prtreqin2	dc.b	1,0,1,0
	dc.w	12,6
	dc.l	0
	dc.l	prtreqla11,0
prtreqla11	dc.b	'Print Text',0
	even
prtreqin3	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	prtreqla16,0
prtreqla16	dc.b	' PRINT ',0
	even
prtreqin4	dc.b	1,0,1,0
	dc.w	4,2
	dc.l	0
	dc.l	prtreqla20,0
prtreqla20	dc.b	' CANCEL ',0
	even

prtreqga1	dc.l	prtreqga2
	dc.w	148,30,192,8,0,1,4
	dc.l	0,0,0,0,prtreqsg1
	dc.w	1
	dc.l	0
prtreqsg1	dc.l	prtname,undobuffer
	dc.w	0,64,0
	dc.w	0,0,0,0,0,0,0
	dc.l	0,0
prtname	dc.b	'PRT:',0
	dcb.b	59,0
	even
prtreqga2	dc.l	prtreqga3
	dc.w	8,52,64,12,0,5,1
	dc.l	prtreqbo4,0,prtreqin3,0,0
	dc.w	2
	dc.l	0
prtreqga3	dc.l	0
	dc.w	280,52,72,12,0,5,1
	dc.l	prtreqbo5,0,prtreqin4,0,0
	dc.w	3
	dc.l	0

font6	incbin	chars.b
font8	incbin	chars8.b

