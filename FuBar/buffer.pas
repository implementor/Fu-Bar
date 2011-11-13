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

{$mode objfpc}{$coperators on}{$longstrings on}

unit buffer;

interface

uses sysutils;

procedure UpdateBuffer(const buf: string);
procedure AppendBuffer(const buf: string);
procedure ClearBuffer;
procedure PushBuffer;
procedure PopBuffer;

procedure UpdatePointer(const pos: word);
procedure ResetPointer;
procedure PushPointer;
procedure PopPointer;
procedure MovePointer(const by: shortint = 1);
function ReadPointer: word;

function ReadBuffer: char;
function ReadRemaining: string;
function PeekBuffer: char;
function CopyBuffer(const s, e: word): string;
procedure CopyPush(const s, e: word);

implementation

type
    TPtrLifo = record
        ptr: word;
        next: pointer;
    end;
    PPtrLifo = ^TPtrLifo;
    TBufferLifo = record
        buf: string;
        ptr: PPtrLifo;
        len: word;
        next: pointer;
    end;
    PBufferLifo = ^TBufferLifo;

var
    fbuffer: string;
    fptr, flen: word;
    ptrlifo: PPtrLifo;
    buflifo: PBufferLifo;
    
procedure UpdateBuffer(const buf: string);
begin
    if buf <> '' then
        fbuffer := buf
    else fbuffer := #10;
    flen := length(buf);
    ResetPointer;
end;

procedure AppendBuffer(const buf: string);
begin
    fbuffer += buf;
    flen += length(buf);
end;

procedure ClearBuffer;
begin
    fbuffer := '';
    flen := 0;
    ResetPointer;
end;

procedure PushBuffer;
var p: PBufferLifo;
begin
    New(p);
    PushPointer;
    p^.ptr := ptrlifo;
    ptrlifo := nil;
    p^.buf := fbuffer;
    p^.len := flen;
    p^.next := buflifo;
    buflifo := p;
end;

procedure PopBuffer;
var p: PBufferLifo;
begin
    p := buflifo^.next;
    while ptrlifo<>nil do
        PopBuffer;
    ptrlifo := buflifo^.ptr;
    fbuffer := buflifo^.buf;
    flen := buflifo^.len;
    Dispose(buflifo);
    buflifo := p;
    PopPointer;
end;

procedure UpdatePointer(const pos: word);
begin
    if pos<flen then fptr := pos
    else fptr := high(fptr)
end;

procedure ResetPointer;
begin
    fptr := 1;
end;

procedure MovePointer(const by: shortint = 1);
begin
    if (fptr+by >= 1) and (fptr+by < flen) then
        fptr += by
    else ResetPointer;
end;

procedure PushPointer;
var p: PPtrLifo;
begin
    New(p);
    p^.ptr := fptr;
    p^.next := ptrlifo;
    ptrlifo := p;
end;

procedure PopPointer;
var p: PPtrLifo;
begin
    p := ptrlifo^.next;
    fptr := ptrlifo^.ptr;
    Dispose(ptrlifo);
    ptrlifo := p;
end;

function ReadBuffer: Char;
begin
    if fptr>flen then
        Result := #10
    else 
        Result := fbuffer[fptr];
    Inc(fptr);
    if fptr >= flen then
        AppendBuffer(#10#10);
    if Result = #13 then
        Result := #10
end;

function PeekBuffer: Char;
begin
    Result := fbuffer[fptr];
end;

function ReadRemaining: string;
var c: char;
begin
    c := ReadBuffer;
    Result := '';
    while c <> #10 do
    begin
        Result += c;
        c := ReadBuffer;
    end;
end;

function ReadPointer: Word;
begin
    Result := fptr
end;

function CopyBuffer(const s, e: word): string;
begin
    Result := Copy(fbuffer,s,e-s);
end;

procedure CopyPush(const s, e: word);
begin
    PushBuffer;
    UpdateBuffer(CopyBuffer(s,e));
end;

initialization
    fptr := 0;
    fbuffer := '';
    flen := 0;
    ptrlifo := nil;
    buflifo := nil;
finalization
    while buflifo <> nil do
        PopBuffer;
    while ptrlifo <> nil do
        PopPointer;
end.
