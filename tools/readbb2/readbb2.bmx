' a blitzmax tool to untokenise old bb2 files

path$="/Users/simon/blitz2/husker3.bb2"

libs$="libs.txt"
tokens$="tokens.txt"

Function HexToDecimal(h$)
	Local t2$=Upper$(Trim$(h$))
	Local d=0
	For Local z=1 To Len(t2$)
		Local i=Instr("0123456789ABCDEF",Mid$(t2$,z,1))
		If i>0 Then d=d*16+i-1
	Next
	Return d
End Function

lines$=LoadText(libs)
libnum=0
cmdnum=0

Global func:String[65536]

For line$=EachIn lines.Split("~n")
	If libnum=0
		p=line.Find("(#")
		libnum=line[p+2..].toInt()		
		cmdnum=libnum*128
	Else
		line=line.Trim()
		If line="" 
			libnum=0
			cmdnum=0
		Else		
			p=line.find(" ")
			If p=-1 p=line.length
'			Print "*"+num+":"+line[..p]	'+"---"+line[p..]
			If func[cmdnum]
				Print "Name Collison"
			EndIf
			func[cmdnum]=line[..p]
			cmdnum:+1
		EndIf
	EndIf
Next

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
				cmd=Int(lhs[3..])
				rhs=rhs[1..rhs.length-1]
				For name$=EachIn rhs.Split(",")					
'					Print cmd +"<-"+name
					func[cmd]=name
					cmd:+1
				Next
			EndIf
		Else
			Print lhs + " : " +rhs
		EndIf
	EndIf
Next

bank:TBank=LoadBank(path)
Print bank.Size()

src$=""

For i=0 Until bank.size()-1

b=bank.PeekByte(i)
b1=bank.PeekByte(i+1)

If b=0 
	src:+"~r~n"
EndIf

If b>31 And b<127 
	src:+Chr(b)
EndIf

If b>127
	cmd=(b&127)*256+b1
	src:+func[cmd]	
''	Print (b&127)+","+b1+","
'	Print "#"+Hex(cmd)+" "+func[cmd]
'	i:+1
EndIf

Next

Print src

'src$=LoadString(path)
'Print src.length




Function ScanBB2(path$)
	For Local dir$=EachIn LoadDir(path)	
		If dir[0]="." Continue
		Local path2$=path+"/"+dir
		Select FileType(path2)
			Case FILETYPE_DIR
				ScanBB2 path2
			Case FILETYPE_FILE
				If dir.ToLower().EndsWith(".bb2")
				
				EndIf
		End Select
	Next
End Function
