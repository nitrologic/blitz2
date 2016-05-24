	errfile	'ram:assem.output'
	objfile	'rawed.obj.new'
;_[]
	SECTION	rawedobjrs000000,CODE
ProgStart
	bra.w	_InitRawEd
	bra.w	_RawKeyConv
	bra.w	_CloseCon
	bra.w	_FreeSignal

consoledevice.MSG	db	'console.device',0
	db	1
_Consolebase	dl	0
lbB000020	db	0
	db	0
lbL000022	dl	0
	dl	$1000000
	dl	0
	dl	0
	dl	0
	dw	0
lbB000038	dcb.b	$69,0
lbB0000A1	dcb.b	$69,0

_FreeSignal
	pushm	d0/d1/a0-a1/a6
	lea	(_MessPort,pc),a0
	moveq	#0,d0
	move.b	(MP_SIGBIT,a0),d0
	cmp.b	#$ff,d0
	beq.b	.exit
	movea.l	(4).w,a6
	jsr	(_LVOFreeSignal,a6)
.exit	popm	d0/d1/a0-a1/a6
	rts

_RawKeyConv	cmpi.w	#'Y',d0
	bhi.w	lbC00012E
	cmpi.w	#'P',d0
	bcs.w	lbC000120
	addi.w	#$31,d0
	rts

lbC000120	cmpi.w	#'L',d0
	bcs.w	lbC00013C
	subi.w	#$30,d0
	rts

lbC00012E	cmpi.w	#'_',d0
	bne.w	lbC00013C
	move.w	#$8B,d0
	rts

lbC00013C	movem.l	a0-a2/a6,-(sp)
	lea	(lbL000022,pc),a0
	move.w	d0,(6,a0)
	move.w	d1,(8,a0)
	lea	(lbB000020,pc),a1
	clr.w	(a1)
	moveq	#1,d1
	suba.l	a2,a2
	movea.l	(_Consolebase,pc),a6
	jsr	(_LVORawKeyConvert,a6)
	moveq	#0,d1
	cmpi.w	#$FFFF,d0
	beq.w	lbC00016C
	move.b	(lbB000020,pc),d1
lbC00016C	move.w	d1,d0
	movem.l	(sp)+,a0-a2/a6
	rts

_InitRawEd	lea	(_IOReq,pc),a2
	movea.l	a2,a0
	move.w	#$28,d0
_clearlp	clr.w	(a0)+
	dbra	d0,_clearlp
	move.b	#NT_MESSAGE,(MN+LN_TYPE,a2)
	move.w	#$30,(MN_LENGTH,a2)
	lea	(_ListStruct,pc),a0
	move.l	a0,(8,a0)
	addq.w	#4,a0
	move.l	a0,(-4,a0)
	movea.l	(4).l,a6
	suba.l	a1,a1
	jsr	(_LVOFindTask,a6)
	lea	(_MessPort,pc),a3
	move.b	#NT_MSGPORT,(LN_TYPE,a3)
	move.l	d0,(MP_SIGTASK,a3)
	moveq	#-1,d0
	jsr	(_LVOAllocSignal,a6)
	move.b	d0,(MP_SIGBIT,a3)	;Need bit here for fail
	moveq	#-1,d1
	cmp.l	d0,d1
	beq.b	_Errorexit
	move.l	a3,(MN_REPLYPORT,a2)
	lea	(consoledevice.MSG,pc),a0
	moveq	#-1,d0
	movea.l	a2,a1
	moveq	#0,d1
	jsr	(_LVOOpenDevice,a6)
	lea	(_IOReq,pc),a0
	lea	(_Consolebase,pc),a1
	move.l	($14,a0),(a1)
	lea	(lbB000038,pc),a0
	lea	(lbB0000A1,pc),a1
	moveq	#0,d2
lbC0001EA	move.w	d2,d0
	moveq	#0,d1
	bsr.w	_RawKeyConv
	move.b	d0,(a0)+
	move.w	d2,d0
	moveq	#1,d1
	bsr.w	_RawKeyConv
	move.b	d0,(a1)+
	addq.w	#1,d2
	cmpi.w	#105,d2
	bne.w	lbC0001EA
	moveq	#0,d0
	rts
_Errorexit
	moveq	#1,d0
	rts

_CloseCon	lea	(_IOReq,pc),a1
	movea.l	(4).l,a6
	jmp	(_LVOCloseDevice,a6)

_IOReq	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
_MessPort	dl	0
	dl	0
	dl	0
	dl	0
	dl	0
_ListStruct	dl	0
	dl	0
	dl	0
	dl	$7000
	dl	$4E75000F

	end
