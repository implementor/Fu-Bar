{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of Fu Bar.
//
//    Fu Bar is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.}

{$mode objfpc}{$longstrings on}{$coperators on}

unit expressions;

interface

uses cradle, sqrt, math, vars, buffer, explain, cmplx;

function RunFunc(const name: string; const x: Complex): Complex;
function FindZero(const a,b: Complex; const name: string; const _f:Complex=0): Complex;
function ShortSum: Complex;
function ShortProduct: Complex;
function Iteration: Complex;
function Factor: Complex;
function Term: Complex;
function TermA: Complex;
function Expression: Complex;
function Eval: Complex;

var
    Answer: Complex;

implementation

type
    TUnit = (uradians, uDegree, uGrad, uTau, uPi, uThousand, uImaginary);
    
var
    xid: cardinal = $00051421; // START in 1337speak
    dontfollow: boolean;

function GetUnit: TUnit;
var nm: string;
begin
    Result := uradians;
    if look='d' then begin
        nm := GetName;
        if nm<>'deg' then Expected('Unit',''''+nm+'''');
        Result := uDegree
    end else if look='r' then begin
        nm := GetName;
        if nm<>'rad' then Expected('Unit',''''+nm+'''')
    end else if look='g' then begin
        nm := GetName;
        if nm<>'grad' then Expected('Unit',''''+nm+'''');
        Result := uGrad
    end else if look='t' then begin
        nm := GetName;
        if (nm<>'tau') and (nm<>'turn') and (nm<>'thousand') then Expected('Unit',''''+nm+'''');
        // Needed for 'nyan thousand':
        if nm = 'thousand' then Result := uThousand
        else Result := uTau
    end else if look='p' then begin
        nm := GetName;
        if (nm<>'pi') then Expected('Unit',''''+nm+'''');
        Result := uPi
    end else if look='i' then begin
        Match('i');
        Result := uImaginary;
    end;
end;

function RunFunc(const name: string; const x: Complex): Complex;
var
    vn,b: string;
    tz: boolean;
    vv: Complex;
    l: char;
    z: boolean;
begin
    if dontfollow then
        Result := 0
    else begin
        if not FuncExists(name) then
            Error('Func '+name+' is undefined!');
        b := GetFunc(name,tz,vn);
        z := (not tz) and (x=Complex(0));
        if z and (not VarExists(name+'0')) then
            Error('Func '+name+' is undefined for '+vn+'=0')
        else if z then
            Result := GetVar(name+'0')
        else begin
            PushBuffer;
            UpdateBuffer(b);
            l := look;
            GetChar;
            vv := GetVar(vn);
            SetVar(vn, x);
            ExplainFuncEnter(name,vn,x);
            IndentExplanations;
            Result := Expression;
            UnindentExplanations;
            ExplainFuncLeave(name,vn,x,result);
            SetVar(vn, vv);
            PopBuffer;
            look := l;
        end;
    end;
end;

function FindZero(const a,b: Complex; const name: string; const _f:Complex=0): Complex;
var f,c: Complex;
begin
    c := (a+b)/2;
    f := RunFunc(name,c);
    if _f <> 0 then begin
        if SameValue(f,_f) then
            Error('Function does not converge.');
    end;
    if f=Complex(0) then
        Result := c
    else if (f < 0) then
        Result := FindZero(c,b,name,f)
    else if (f > 0) then
        Result := FindZero(a,c,name,f)
end;

function Shortsum: Complex;
var
    x, y, step, vx: Complex; 
    ret: word; 
    l, rl: char;
    v: string;
begin
    Match('(');
    SkipWHite;
    v := GetName;
    SkipWHite;
    Match(':');
    x := Expression;
    Match('<');
    y := Expression;
    if look = '$' then begin
        Match('$');
        step := Expression;
    end else step := 1;
    Match(';');
    Result := 0;
    l := look;
    if x >= y then begin
        vx := GetVar(v);
        SetVar(v,0);
        Expression;
        SetVar(v,vx);
    end else begin
        vx := GetVar(v);
        while x <= y do begin
            SetVar(v,x);
            PushPointer;
            ExplainSumEnter(x,v);
            IndentExplanations;
            Result += Expression;
            UnindentExplanations;
            ExplainSumLeave(x,Result,v);
            ret := ReadPointer;
            rl := look;
            PopPointer;
            look := l;
            x += step;
        end;
        SetVar(v,vx);
        UpdatePointer(ret);
        look := rl;
    end;
    Match(')');
end;

function ShortProduct: Complex;
var
    x, y, step, vx: Complex; 
    ret: word; 
    l, rl: char;
    v: string;
begin
    Match('(');
    SkipWHite;
    v := GetName;
    SkipWHite;
    Match(':');
    x := Expression;
    Match('<');
    y := Expression;
    if look = '$' then begin
        Match('$');
        step := Expression;
    end else step := 1;
    Match(';');
    Result := 1;
    l := look;
    if x >= y then begin
        vx := GetVar(v);
        SetVar(v,1);
        Expression;
        SetVar(v,vx)
    end else begin
        vx := GetVar(v);
        while x <= y do begin
            SetVar(v,x);
            PushPointer;
            ExplainProdEnter(x,v);
            IndentExplanations;
            Result *= Expression;
            UnindentExplanations;
            ExplainProdLeave(x,Result,v);
            ret := ReadPointer;
            rl := look;
            PopPointer;
            look := l;
            x += step;
        end;
        SetVar(v,vx);
        UpdatePointer(ret);
        look := rl;
    end;
    Match(')');
end;

function Iteration: Complex;
var 
    x, xn: Complex;
    l, rl: char;
    p: word;
begin
    Match('(');
    xn := Expression;
    Match(':');
    l := look;
    repeat
      x := xn;
      SetVar('xn',xn);
      PushPointer;
      ExplainIterEnter(xn);
      IndentExplanations;
      xn := Expression;
      UnindentExplanations;
      ExplainIterLeave(xn);
      p := ReadPointer;
      rl := look;
      PopPointer;
      look := l;
    until (xn=x) or dontfollow;
    look := rl;
    UpdatePointer(p);
    Match(')');
    Result := xn;
    ExplainIterFix(xn);
end;

function Factor: Complex;
var 
    nm, t, t2, l, r: string; 
    k: byte; 
    u: TUnit; 
    x,y: Complex;
    b,c: boolean;
    a,e: word;
begin
    SkipWhite;
    if look = '(' then
    begin
        Match('(');
        Result := Expression;
        Match(')')
    end else if look = '|' then begin
        Match('|');
        Result := Expression;
        ExplainAbs(Result);
        Result := RunFunc('_abs',Result);
        TellResult(Result);
        Match('|')
    end else if look = '?' then begin
        Match('?');
        Match('(');
        Result := Expression;
        b := true;
        c := SameValue(Result,0);
        if look=':' then begin
            Match(':');
            dontfollow := c;
            x := Expression;
            b := false;
        end else x := Result;
        if (look=';') or b then begin
            Match(';');
            dontfollow := not c;
            y := Expression;
        end else y := 0;
        Match(')');
        dontfollow := false;
        if c then Result := y
        else Result := x;
    end else if look = '{' then begin
        Match('{');
        SkipWhite;
        if look='I' then begin
            Match('I'); 
            if look='n' then begin
                Match('n'); Match('t');
                Result := MakeInt(Expression.r);
            end else begin
                Match('m');
                Result := Expression.i;
            end
        end else if look='R' then begin
            Match('R'); Match('e');
            Result := Expression.r;
        end;
        Match('}')
    end else if IsAlpha(look) then begin
        nm := GetName;
        if nm = '_sqrt' then begin
            Match('(');
            Result := Expression;
            Match(')');
            Result := sqrt.heron(Result)
        end else if (nm = 'nrt') or (nm = 'Root') then begin
            Match('(');
            Result := Expression;
            Match(';');
            k := MakeInt(Expression);
            Match(')');
            Result := sqrt.heron(Result,k)
        end else if nm = '_mof' then begin
            Match('(');
            x := Expression;
            Match(':');
            y := Expression;
            if y<0 then y := -y;
            Match(')');
            while (not SameValue(0,y)) and (y>0) do
                y -= x;
            if SameValue(0,y) then Result := 1
            else Result := 0;
        end else if nm = 'gcd' then begin
            Match('(');
            Result := Expression;
            Match(';');
            x := Expression;
            Match(')');
            Result := gcd(MakeInt(Result),MakeInt(x))
        end else if nm = 'lcm' then begin
            Match('(');
            Result := Expression;
            Match(';');
            x := Expression;
            Match(')');
            Result := lcm(MakeInt(Result),MakeInt(x))
        end else if nm = 'zero' then begin
            Match('(');
            SkipWhite;
            t := GetName;
            SkipWhite;
            Match(':');
            a := ReadPointer;
            SetVar(t,0);
            Expression;
            e := ReadPointer;
            t2 := '_'+numtostr(xid);
            Inc(xid);
            SetFunc(t2,CopyBuffer(a-1,e-1),true,t);
            Result := FindZero(-1000000,1000000,t2);
            //Result := RunFunc(t2, 45);
            Match(')');
        end else if nm = 'solve' then begin
            Match('(');
            SkipWhite;
            t := GetName;
            SkipWhite;
            Match(':');
            a := ReadPointer;
            SetVar(t,0);
            Expression;
            e := ReadPointer;
            t2 := '_'+numtostr(xid);
            Inc(xid);
            l := CopyBuffer(a-1,e-1);
            Match('=');
            SkipWhite;
            a := ReadPointer;
            SetVar(t,0);
            Expression;
            e := ReadPointer;
            r := CopyBuffer(a-1,e-1);
            Match(')');
            SetFunc(t2,'('+l+')-('+r+')',true,t);
            Result := FindZero(-100,100,t2);
        end else if nm = 'sum' then
            Result := Shortsum
        else if nm = 'product' then
            Result := ShortProduct
        else if nm = 'iterate' then
            Result := Iteration
        else if nm = 'ans' then
            Result := answer
        else begin
            if (look='(') and FuncExists(nm) then begin
                Match('(');
                x := Expression;
                Match(')');
                Result := RunFunc(nm,x);
            end else if (look<>'(') and VarExists(nm) then
                Result := GetVar(nm)
            else Expected('Valid function name', 'undefined const '''+nm+'''')
        end
    end else
        Result := GetNum;
    SkipWhite;
    u := GetUnit;
    if u<>uRadians then
        ExplainConv(Result,ord(u));
    case u of
    uDegree:    Result := (Result / 180) * Pi;
    uGrad:      Result := (Result / 200) * Pi;
    uTau:       Result := Result * Pi * 2;
    uPi:        Result := Result * Pi;
    uImaginary: Result := Result * cmplx_i;
    // Needed for 'nyan thousand':
    uThousand:  Result := Result * 1000;
    end;
    if u<>uRadians then
        TellResult(Result);
    SkipWhite;
    while look = '!' do begin
        Result := RunFunc('_fac',Result);
        Match('!');
        SkipWhite
    end;
    if look = '^' then
    begin
        Match('^');
        x := Factor();
        ExplainPower(Result,x);
        Result := sqrt.Power(Result, MakeInt(x));
        TellResult(Result)
    end;
    SkipWhite
end;

function Term: Complex;
var a: Complex;
begin
    Result := Factor;
    while look in ['*','/','\','#'] do
        case look of
        '*': begin Match('*'); a := Factor; ExplainMul(Result,a); Result *= a; TellResult(Result) end;
        '/': begin Match('/'); a := Factor; ExplainDiv(Result,a); if dontfollow then a := 1; Result /= a; TellResult(Result) end;
        '\': begin Match('\'); a := Factor; ExplainIntDiv(Result,a); if dontfollow then a := 1; Result := MakeInt(Result) div MakeInt(a); TellResult(Result) end;
        '#': begin Match('#'); a := Factor; ExplainMod(Result,a); if dontfollow then a := 1; Result := MakeInt(Result) mod MakeInt(a); TellResult(Result) end;
        end;
    SkipWhite
end;

function TermA: Complex;
var nm: string; a: Complex;
begin
    SkipWhite;
    if look in ['+','-'] then Result := 0
    else Result := Term;
    while look in ['+','-','a','o','x'] do
        case look of
        '+': begin Match('+'); a := Term; ExplainAdd(Result,a); Result += a; TellResult(Result) end;
        '-': begin Match('-'); a := Term; ExplainSub(Result,a); Result -= a; TellResult(Result) end;
        'o': begin
               nm := GetName;
               if nm<>'or' then Expected('Operator',''''+nm+'''');
               a := Term;
               ExplainOr(Result,a);
               Result := MakeInt(Result) or MakeInt(a);
               TellResult(Result)
             end;
        'a': begin
               nm := GetName;
               if nm<>'and' then Expected('Operator',''''+nm+'''');
               a := Term;
               ExplainAnd(Result,a);
               Result := MakeInt(Result) and MakeInt(a);
               TellResult(Result)
             end;
        'x': begin
               nm := GetName;
               if nm<>'xor' then Expected('Operator',''''+nm+'''');
               a := Term;
               ExplainXor(Result,a);
               Result := MakeInt(Result) xor MakeInt(Term);
               TellResult(Result)
             end;
        end;
    SkipWhite
end;

function Expression: Complex;
var nm: string; b: Complex;
begin
    Result := TermA;
    while look in ['f','C'] do
    begin
        case look of
        'f':begin
                nm := GetName;
                if nm <> 'from' then Expected('Operator',''''+nm+'''');
                SkipWhite;
                b := TermA;
                ExplainBinCoef(b,Result);
                Result := BinCoef(MakeInt(b),MakeInt(Result));
                TellResult(Result)
            end;
        'C':begin
                Match('C');
                b := TermA;
                ExplainBinCoef(Result,b);
                Result := BinCoef(MakeInt(Result),MakeInt(b));
                TellResult(Result)
            end;
        end
    end;
    SkipWhite;
end;

function Eval: Complex;
var nm, v, arg: string; f, tz, c: boolean;
begin
    SkipWhite;
    f := false;
    if look='[' then begin
        Match('[');
        SkipWhite;
        if look='!' then begin
            Match('!');
            c := true;
            SkipWhite
        end else c := false;
        v := GetName;
        SkipWhite;
        if look='(' then begin
            f := true;
            tz := true;
            Match('(');
            SkipWhite;
            if look='0' then begin
                Match('0');
                arg := '';
            end else arg := GetName;
            SkipWhite;
            if arg = 'nonzero' then begin
                tz := false;
                arg := GetName;
                SkipWhite
            end;
            Match(')');
            SkipWhite
        end;
        Match(']')
    end else v := '';
    if (not f) or (arg='') then begin
        Result := Expression;
        if f then
            SetVar(v+'0', Result,c)
        else if v <> '' then
            SetVar(v, Result,c);
    end else begin
        nm := ReadRemaining;
        look := ReadBuffer;
        SetFunc(v,nm,tz,arg,c);
        if tz then
            Result := RunFunc(v,0)
        else
            Result := RunFunc(v,1)
    end
end;

initialization
    dontfollow := false;
end.
