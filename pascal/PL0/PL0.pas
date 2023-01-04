(********************* PL0 ±‡“Î≥Ã–ÚTurbo Pascal¥˙¬Î *********************)
program pl0(fa,fa1,fa2); 
(* PL0 compile with code generation *) 
  
label 99; 
      (* Turbo Pascal do not support goto between different 
         blocks so, the 'goto' command in getch are replaced 
         by procedure exitp !! in another way, 'label 99' do 
         not work !!                  Lin Wei       2001  *) 
  
const norw=13;       (* of reserved words *) 
      txmax=100;     (* length of identifier table *) 
      nmax=14;       (* max number of digits in numbers *) 
      al=10;         (* length of identifiers *) 
      amax=2047;     (* maximum address *) 
      levmax=3;      (* max depth of block nesting *) 
      cxmax=200;     (* size of code array *) 
  
type symbol=(nul,ident,number,plus,minus,times,slash,oddsym, 
             eql,neq,lss,leq,gtr,geq,lparen,rparen,comma, 
             semicolon,period,becomes,beginsym,endsym,ifsym, 
             thensym,whilesym,writesym,readsym,dosym,callsym, 
             constsym,varsym,procsym); 
     alfa=packed array[1..al] of char; 
     objects=(constant,variable,procedur); 
     (* wirth used the word "procedure"and"object" there, which won't work! *) 
     symset=set of symbol; 
     fct=(lit,opr,lod,sto,cal,int,jmp,jpc); 
     instruction=packed record 
                    f:fct;        (* function code *) 
                    l:0..levmax;  (* level *) 
                    a:0..amax;    (* displacement addr *) 
                 end; 
              (* lit 0,a load constant a 
                 opr 0,a execute opr a 
                 lod 1,a load variable 1,a 
                 sto 1,a store variable 1,a 
                 cal 1,a call procedure at level 1 
                 int 0,a increment t -register by a 
                 jmp 0,a jump to a 
                 jpc 0,a jump conditional to a *) 
  
var fa:text; 
    fa1,fa2:text; 
    listswitch:boolean;    (* true set list object code *) 
    ch:char;               (* last char read *) 
    sym:symbol;            (* last symbol read *) 
    id:alfa;               (* last identifier read *) 
    num:integer;           (* last number read *) 
    cc:integer;            (* character count *) 
    ll:integer;            (* line length *) 
    kk:integer; 
    cx:integer;            (* code allocation index *) 
    line:array[1..81] of char; 
    a:alfa; 
    code:array[0..cxmax] of instruction; 
    word:array[1..norw] of alfa; 
    wsym:array[1..norw] of symbol; 
    ssym:array[' '..'^'] of symbol; 
        (* wirth uses "array[char]" here *) 
    mnemonic:array[fct] of packed array[1..5] of char; 
    declbegsys, statbegsys, facbegsys:symset; 
    table:array[0..txmax] of record 
            name:alfa; 
            case kind:objects of 
              constant:(val:integer); 
              variable,procedur:(level,adr,size:integer) 
            (* "size" lacking in original. I think it belongs here *) 
          end; 
    fin,fout:text; 
    fname:string; 
    err:integer; 
    endf:boolean; 
  
procedure error(n:integer); 
begin 
  writeln('****','':cc-1,'!',n:2); 
  writeln(fa1,'****','':cc-1,'!',n:2); 
  err:=err+1; 
end; (* error *) 
  
procedure exitp; 
begin 
  endf:=true; 
  close(fin); 
  writeln; 
  exit; 
end; 
  
procedure getsym; 
var i,j,k:integer; 
  
  procedure getch; 
  begin 
    if cc=ll then begin 
      if eof(fin) then begin 
         write('program incomplete'); 
         close(fin); 
         writeln; 
         exitp; 
         (*goto 99;*) 
      end; 
      ll:=0; 
      cc:=0; 
      write(cx:4,' '); 
      write(fa1,cx:4,' '); 
      while not eoln(fin) do begin 
        ll:=ll+1; 
        read(fin,ch); 
        write(ch); 
        write(fa1,ch); 
        line[ll]:=ch; 
      end; 
      writeln; 
      ll:=ll+1; 
      (* read(fin,line[ll]); repleaced by two lines below *) 
      line[ll]:=' '; 
      readln(fin); 
      writeln(fa1); 
    end; 
    cc:=cc+1; 
    ch:=line[cc]; 
  end; (* getch *) 
  
begin (* getsym *) 
  while ch=' ' do getch; 
  if ch in ['a'..'z'] then begin 
     k:=0; 
     repeat 
       if k<al then begin 
          k:=k+1; 
          a[k]:=ch; 
       end; 
       getch; 
     until not(ch in ['a'..'z','0'..'9']); 
     if k>=kk then kk:=k 
     else repeat 
            a[kk]:=' '; 
            kk:=kk-1; 
          until kk=k; 
     id:=a; 
     i:=1; 
     j:=norw; 
     repeat 
       k:=(i+j) div 2; 
       if id<=word[k] then j:=k-1; 
       if id>=word[k] then i:=k+1; 
     until i>j; 
     if i-1>j then sym:=wsym[k] else sym:=ident; 
  end else if ch in ['0'..'9'] then begin (* number *) 
    k:=0; 
    num:=0; 
    sym:=number; 
    repeat 
      num:=10*num+(ord(ch)-ord('0')); 
      k:=k+1; 
      getch; 
    until not(ch in['0'..'9']); 
    if k>nmax then error(30); 
  end else if ch=':' then begin 
    getch; 
    if ch='=' then begin 
       sym:=becomes; 
       getch; 
    end else sym:=nul; 
  end else if ch='<' then begin 
    getch; 
    if ch='=' then begin 
       sym:=leq; 
       getch; 
    end else sym:=lss; 
  end else if ch='>' then begin 
    getch; 
    if ch='=' then begin 
       sym:=geq; 
       getch; 
    end else sym:=gtr; 
  end else begin 
    sym:=ssym[ch]; 
    getch; 
  end; 
end; (* getsym *) 
  
procedure gen(x:fct;y,z:integer); 
begin 
  if cx>cxmax then begin 
     write('program too long'); 
     (*goto 99;*) 
  end; 
  with code[cx] do begin 
       f:=x; 
       l:=y; 
       a:=z; 
  end; 
  cx:=cx+1; 
end; (* gen *) 
  
procedure test(s1,s2:symset;n:integer); 
begin 
  if not(sym in s1) then begin 
     error(n); 
     s1:=s1+s2; 
     while not(sym in s1) do getsym; 
  end; 
end; (* test *) 
  
procedure block(lev,tx:integer;fsys:symset); 
var dx:integer;   (* data allocation index *) 
    tx0:integer;  (* inital table index *) 
    cx0:integer;  (* inital code index *) 
    procedure enter(k:objects); 
    begin (* enter object into table *) 
      tx:=tx+1; 
      with table[tx] do begin 
        name:=id; 
        kind:=k; 
        case k of 
          constant: begin 
                      if num>amax then begin error(31); num:=0; end; 
                      val:=num; 
                    end; 
          variable: begin 
                      level:=lev; 
                      adr:=dx; 
                      dx:=dx+1; 
                    end; 
          procedur: level:=lev; 
        end; 
      end; 
    end; (* enter *) 
  
    function position(id:alfa):integer; 
    var i:integer; 
    begin (* find identifier in table *) 
      table[0].name:=id; 
      i:=tx; 
      while table[i].name<>id do i:=i-1; 
      position:=i; 
    end; (* position *) 
  
    procedure constdeclaration; 
    begin 
      if sym=ident then begin 
         getsym; 
         if sym in [eql,becomes] then begin 
            if sym=becomes then error(1); 
            getsym; 
            if sym=number then begin 
               enter(constant); 
               getsym; 
            end else error(2); 
         end else error(3); 
      end else error(4); 
    end; (* constdeclaration *) 
  
    procedure vardeclaration; 
    begin 
      if sym=ident then begin 
         enter(variable); 
         getsym; 
      end else error(4); 
    end; (* vardeclaration *) 
  
    procedure listcode; 
    var i:integer; 
    begin 
      if listswitch then begin 
         for i:=cx0 to cx-1 do 
             with code[i] do begin 
                  writeln(i,mnemonic[f]:5,l:3,a:5); 
                  writeln(fa,i:4,mnemonic[f]:5,l:3,a:5); 
             end; 
      end; 
    end; (* listcode *) 
  
    procedure statement(fsys:symset); 
    var i,cx1,cx2:integer; 
        procedure expression(fsys:symset); 
        var addop:symbol; 
            procedure term(fsys:symset); 
            var mulop:symbol; 
                procedure factor(fsys:symset); 
                var i:integer; 
                begin 
                  test(facbegsys,fsys,24); 
                  while sym in facbegsys do begin 
                    if sym=ident then begin 
                       i:=position(id); 
                       if i=0 then error(11) 
                       else with table[i] do 
                         case kind of 
                           constant:gen(lit,0,val); 
                           variable:gen(lod,lev-level,adr); 
                           procedur:error(21); 
                         end; 
                       getsym; 
                    end else if sym=number then begin 
                        if num>amax then begin 
                           error(31); 
                           num:=0; 
                        end; 
                        gen(lit,0,num); 
                        getsym; 
                    end else if sym=lparen then begin 
                        getsym; 
                        expression([rparen]+fsys); 
                        if sym=rparen then getsym 
                        else error(22); 
                    end; 
                    test(fsys,facbegsys,23); 
                  end; 
                end; (* factor *) 
  
            begin (* term *) 
              factor([times,slash]+fsys); 
              while sym in [times,slash] do begin 
                mulop:=sym; 
                getsym; 
                factor(fsys+[times,slash]); 
                if mulop=times then gen(opr,0,4) else gen(opr,0,5) 
              end; 
            end; (* term *) 
  
        begin (* expression *) 
          if sym in [plus,minus] then begin 
             addop:=sym; 
             getsym; 
             term(fsys+[plus,minus]); 
             if addop=minus then gen(opr,0,1); 
          end else term(fsys+[plus,minus]); 
          while sym in [plus,minus] do begin 
            addop:=sym; 
            getsym; 
            term(fsys+[plus,minus]); 
            if addop=plus then gen(opr,0,2) else gen(opr,0,3); 
          end; 
        end; (* expression *) 
  
        procedure condition(fsys:symset); 
        var relop:symbol; 
        begin 
          if sym=oddsym then begin 
             getsym; 
             expression(fsys); 
             gen(opr,0,6); 
          end else begin 
             expression([eql,neq,lss,leq,gtr,geq]+fsys); 
             if not(sym in [eql,neq,lss,leq,gtr,geq]) then error(20) 
             else begin 
               relop:=sym; 
               getsym; 
               expression(fsys); 
               case relop of 
                 eql:gen(opr,0,8); 
                 neq:gen(opr,0,9); 
                 lss:gen(opr,0,10); 
                 geq:gen(opr,0,11); 
                 gtr:gen(opr,0,12); 
                 leq:gen(opr,0,13); 
               end; 
             end; 
          end; 
        end; (* condition *) 
  
    begin (* statement *) 
      if sym=ident then begin 
         i:=position(id); 
         if i=0 then error(11) 
         else if table[i].kind<>variable then begin 
           error(12); 
           i:=0; 
         end; 
         getsym; 
         if sym=becomes then getsym else error(13); 
         expression(fsys); 
         if i<>0 then with table[i] do gen(sto,lev-level,adr); 
      end else if sym=readsym then begin 
         getsym; 
         if sym<>lparen then error(34) 
         else repeat 
                getsym; 
                if sym=ident then i:=position(id) 
                else i:=0; 
                if i=0 then error(35) 
                else with table[i] do begin 
                  gen(opr,0,16); 
                  gen(sto,lev-level,adr); 
                end; 
                getsym; 
         until sym<>comma; 
         if sym<>rparen then begin 
            error(33); 
            while not(sym in fsys) do getsym; 
         end else getsym; 
      end else if sym=writesym then begin 
         getsym; 
         if sym=lparen then begin 
            repeat 
              getsym; 
              expression([rparen,comma]+fsys); 
              gen(opr,0,14); 
            until sym<>comma; 
            if sym<>rparen then error(33) else getsym; 
         end; 
         gen(opr,0,15); 
      end else if sym=callsym then begin 
         getsym; 
         if sym<>ident then error(14) 
         else begin 
           i:=position(id); 
           if i=0 then error(11) else with table[i] do 
              if kind=procedur then gen(cal,lev-level,adr) 
              else error(15); 
           getsym; 
         end; 
      end else if sym=ifsym then begin 
         getsym; 
         condition([thensym,dosym]+fsys); 
         if sym=thensym then getsym 
         else error(16); 
         cx1:=cx; 
         gen(jpc,0,0); 
         statement(fsys); 
         code[cx1].a:=cx; 
      end else if sym=beginsym then begin 
         getsym; 
         statement([semicolon,endsym]+fsys); 
         while sym in [semicolon]+statbegsys do begin 
           if sym=semicolon then getsym 
           else error(10); 
           statement([semicolon,endsym]+fsys); 
         end; 
         if sym=endsym then getsym else error(17); 
      end else if sym=whilesym then begin 
         cx1:=cx; 
         getsym; 
         condition([dosym]+fsys); 
         cx2:=cx; 
         gen(jpc,0,0); 
         if sym=dosym then getsym else error(18); 
         statement(fsys); 
         gen(jmp,0,cx1); 
         code[cx2].a:=cx; 
      end; 
      test(fsys,[],19); 
    end; (* statement *) 
  
begin (* block *) 
  dx:=3; 
  tx0:=tx; 
  table[tx].adr:=cx; 
  gen(jmp,0,0); 
  if lev>levmax then error(32); 
  repeat 
    if sym=constsym then begin 
       getsym; 
       repeat 
         constdeclaration; 
         while sym=comma do begin 
           getsym; 
           constdeclaration; 
         end; 
         if sym=semicolon then getsym else error(5); 
       until sym<>ident; 
    end; 
    if sym=varsym then begin 
       getsym; 
       repeat; 
         vardeclaration; 
         while sym=comma do begin 
           getsym; 
           vardeclaration; 
         end; 
         if sym=semicolon then getsym else error(5); 
       until sym<>ident; 
    end; 
    while sym=procsym do begin 
      getsym; 
      if sym=ident then begin 
         enter(procedur); 
         getsym; 
      end else error(4); 
      if sym=semicolon then getsym else error(5); 
      block(lev+1,tx,[semicolon]+fsys); 
      if sym=semicolon then begin 
         getsym; 
         test(statbegsys+[ident,procsym],fsys,6); 
      end else error(5); 
    end; 
    test(statbegsys+[ident],declbegsys,7); 
  until not(sym in declbegsys); 
  code[table[tx0].adr].a:=cx; 
  with table[tx0] do begin 
       adr:=cx; 
       size:=dx; 
  end; 
  cx0:=cx; 
  gen(int,0,dx); 
  statement([semicolon,endsym]+fsys); 
  gen(opr,0,0); 
  test(fsys,[],8); 
  listcode; 
end; (* block *) 
  
procedure interpret; 
const stacksize=500; 
var p,b,t:integer; (* program base topstack registers *) 
    i:instruction; 
    s:array[1..stacksize] of integer; (* datastore *) 
  
    function base(l:integer):integer; 
    var bl:integer; 
    begin 
      bl:=b; (* find base 1 level down *) 
      while l>0 do begin 
        bl:=s[bl]; 
        l:=l-1; 
      end; 
      base:=bl; 
    end; (* base *) 
  
begin 
  writeln('start pl0'); 
  t:=0; b:=1; p:=0; 
  s[1]:=0; s[2]:=0; s[3]:=0; 
  repeat 
    i:=code[p]; 
    p:=p+1; 
    with i do case f of 
      lit: begin t:=t+1; s[t]:=a; end; 
      opr: case a of (* operator *) 
             0: begin (* return *) 
                  t:=b-1; 
                  p:=s[t+3]; 
                  b:=s[t+2]; 
                end; 
             1: s[t]:=-s[t]; 
             2: begin t:=t-1; s[t]:=s[t]+s[t+1]; end; 
             3: begin t:=t-1; s[t]:=s[t]-s[t+1]; end; 
             4: begin t:=t-1; s[t]:=s[t]*s[t+1]; end; 
             5: begin t:=t-1; s[t]:=s[t] div s[t+1]; end; 
             6: s[t]:=ord(odd(s[t])); 
             8: begin t:=t-1; s[t]:=ord(s[t]=s[t+1]); end; 
             9: begin t:=t-1; s[t]:=ord(s[t]<>s[t+1]); end; 
             10:begin t:=t-1; s[t]:=ord(s[t]<s[t+1]); end; 
             11:begin t:=t-1; s[t]:=ord(s[t]>=s[t+1]); end; 
             12:begin t:=t-1; s[t]:=ord(s[t]>s[t+1]); end; 
             13:begin t:=t-1; s[t]:=ord(s[t]<=s[t+1]); end; 
             14:begin write(s[t]); write(fa2,s[t]); t:=t-1; end; 
             15:begin writeln; writeln(fa2); end; 
             16:begin t:=t+1; write('?'); write(fa2,'?'); readln(s[t]); 
                writeln(fa2,s[t]); end; 
           end; 
      lod: begin t:=t+1; s[t]:=s[base(l)+a]; end; 
      sto: begin s[base(l)+a]:=s[t]; (* writeln(s[t]) *) t:=t-1; end; 
      cal: begin (* generat new block mark *) s[t+1]:=base(l); s[t+2]:=b; 
           s[t+3]:=p; b:=t+1; p:=a; end; 
      int: t:=t+a; 
      jmp: p:=a; 
      jpc: begin if s[t]=0 then p:=a; t:=t-1; end; 
    end; (* with, case *) 
  until p=0; 
  close(fa2); 
end; (* interpret *) 
  
begin (* main *) 
  for ch:=' ' to '!' do ssym[ch]:=nul; 
  (* changed bacause of different character set 
     note the typos below in the original where 
     the alfas were not given the correct space *) 
  word[1]:='begin     ';  word[2]:='call      '; 
  word[3]:='const     ';  word[4]:='do        '; 
  word[5]:='end       ';  word[6]:='if        '; 
  word[7]:='odd       ';  word[8]:='procedure '; 
  word[9]:='read      ';  word[10]:='then      '; 
  word[11]:='var       '; word[12]:='while     '; 
  word[13]:='write     '; 
  
  wsym[1]:=beginsym;   wsym[2]:=callsym; 
  wsym[3]:=constsym;   wsym[4]:=dosym; 
  wsym[5]:=endsym;     wsym[6]:=ifsym; 
  wsym[7]:=oddsym;     wsym[8]:=procsym; 
  wsym[9]:=readsym;    wsym[10]:=thensym; 
  wsym[11]:=varsym;    wsym[12]:=whilesym; 
  wsym[13]:=writesym; 
  
  ssym['+']:=plus;     ssym['-']:=minus; 
  ssym['*']:=times;    ssym['/']:=slash; 
  ssym['(']:=lparen;   ssym[')']:=rparen; 
  ssym['=']:=eql;      ssym[',']:=comma; 
  ssym['.']:=period;   ssym['#']:=neq; 
  ssym[';']:=semicolon; 
  
  mnemonic[lit]:='lit  ';  mnemonic[opr]:='opr  '; 
  mnemonic[lod]:='lod  ';  mnemonic[sto]:='sto  '; 
  mnemonic[cal]:='cal  ';  mnemonic[int]:='int  '; 
  mnemonic[jmp]:='jmp  ';  mnemonic[jpc]:='jpc  '; 
  
  declbegsys:=[constsym,varsym,procsym]; 
  statbegsys:=[beginsym,callsym,ifsym,whilesym]; 
  facbegsys:=[ident,number,lparen]; 
  
  (* page(output) *) 
  endf:=false; 
  assign(fa1,'PL0.txt'); 
  rewrite(fa1); 
  write('input file? '); 
  write(fa1,'input file?'); 
  readln(fname); 
  writeln(fa1,fname); 
  (* openf(fin,fname,'r'); ==> *) 
  assign(fin,fname); reset(fin); 
  write('list object code ?'); 
  readln(fname); 
  write(fa1,'list object code ?'); 
  listswitch:=(fname[1]='y'); 
  err:=0; 
  cc:=0; cx:=0; ll:=0; 
  ch:=' '; kk:=al; 
  getsym; 
  assign(fa,'PL0-1.txt'); 
  assign(fa2,'PL0-2.txt'); 
  rewrite(fa); 
  rewrite(fa2); 
  block(0,0,[period]+declbegsys+statbegsys); 
  close(fa); 
  close(fa1); 
  if sym<>period then error(9); 
  if err=0 then interpret else write('error in pl/0 program'); 
99: (* this line is not work in turbo pascal so replace by 
       procedure exitp: see the memo at the top *) 
  close(fin); 
  writeln; 
end. 
  
  

  
  
