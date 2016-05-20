' a blitzmax tool to untokenise old bb2 files
' by simon 

Global func:String[65536]

PopulateFuncTable

path$="/Users/simon/blitz2/husker3.bb2"
src$=ReadBlitz2Source(path)
SaveText src,"test.txt"

'src$=LoadString(path)
'Print src.length

ScanBB2("/Users/simon/Google Drive/amiga")

Function ScanBB2(path$)
	For Local dir$=EachIn LoadDir(path)	
		If dir[0]="." Continue
		Local path2$=path+"/"+dir
		Select FileType(path2)
			Case FILETYPE_DIR
				ScanBB2 path2
			Case FILETYPE_FILE
				Local src$=dir.ToLower()
				If src.EndsWith(".bb2") Or src.EndsWith(".bb")					
					Local dest$="scan/"+src+".txt"
					bb2$=ReadBlitz2Source(path2)
					Print dest													
					bb2="; ascii translation of "+path2+"~r~n+"+bb2
					SaveText bb2,dest	
'					Print bb2				
				EndIf
		End Select
	Next
End Function

Function ReadBlitz2Source$(path$)

	bank:TBank=LoadBank(path)
	Print bank.Size()
	
	src$=""
	lost$=""
	
	For i=0 Until bank.size()-1
	
		b=bank.PeekByte(i)
		b1=bank.PeekByte(i+1)
		
		If b=0 
			src:+"~r~n"
		EndIf
		
		If b=32 And b1=32
			src:+"~t"
			i:+1
			Continue
		EndIf
			
		
		If b>31 And b<127 
			src:+Chr(b)
		EndIf
		
		If b<32 And b>0
			Print b
		EndIf
		
		If b>127
			cmd=(b Shl 8)|(b1&255)
			i:+1
			token$=func[cmd]	
			If token="" 
				token="##[$"+Hex(cmd)[4..]+"]"
				If Not lost.Contains(token)
					lost:+token
				EndIf
			EndIf
			src:+token	
		EndIf
	
	Next
	
	Print "Lost Tokens:"+lost
	
	Return src
End Function
	
Function HexToDecimal(h$)
	Local t2$=Upper$(Trim$(h$))
	Local d=0
	For Local z=1 To Len(t2$)
		Local i=Instr("0123456789ABCDEF",Mid$(t2$,z,1))
		If i>0 Then d=d*16+i-1
	Next
	Return d
End Function

Function PopulateFuncTable()
	libs$="libs.txt"
	tokens$="tokens.txt"
	opcodes$="opcodes.txt"
	
	' asm opcodes
	
	cmdnum=60+$8000
	lines$=LoadText(opcodes)
	For line$=EachIn lines.Split("~n")
		p=line.find("=")
		pr=line.find(",")
		op$=line[p+1..pr]
	'	Print cmdnum+op
		func[cmdnum]=op
		cmdnum:+1
	Next
	
	lines$=LoadText(libs)
	libnum=0
	cmdnum=0
	For line$=EachIn lines.Split("~n")
		If libnum=0
			p=line.Find("(#")
			libnum=line[p+2..].toInt()		
			cmdnum=(libnum+256)*128
			Print line +"->"+ libnum +"->"+ Hex(cmdnum)
		Else
			line=line.Trim()
			If line="" 
				libnum=0
				cmdnum=0
			Else		
				cmdnum:+1
				p=line.find(" ")
				If p=-1 p=line.length
	'			Print "$"+Hex(cmdnum)[4..]+":"+line[..p]	'+"---"+line[p..]
				If func[cmdnum]
					Print "Name Collison"
				EndIf
				func[cmdnum]=line[..p]
			EndIf
		EndIf
	Next
	
	' tokens from source
	
	lines$=LoadText(tokens)
	For token$=EachIn lines.Split("~n")
		token=token.Trim()
		If token[..1]=";" Continue
		p=token.find("=")
		If p<>-1
			lhs$=token[..p]
			rhs$=token[p+1..]		
			If rhs[..1]="$"
				cmd=HexToDecimal(rhs[1..])
	'			Print cmd+"="+lhs			
				func[cmd]=lhs
			ElseIf rhs[..1]="~q"
				If lhs[..3]="tok"
					cmd=Int(lhs[3..])+$8000
					rhs=rhs[1..rhs.length-1]
					For name$=EachIn rhs.Split(",")					
	'					Print cmd +"<-"+name
						func[cmd]=name
						cmd:+1
					Next
				EndIf
			Else
				Print lhs + " : " +rhs + " "+Hex(Int(rhs))
			EndIf
		EndIf
	Next

	Return
End Function


