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

unit tree;

interface

type
    // Rough
    TNode = class(TObject);
    TBinOp = class(TNode)
        AOperand, BOperand: TNode;
        Operation: Byte;
    end;
    TUnOp = class(TNode)
        Operand: TNode;
        Operation: Byte;
    end;
    TFuncCall = class(TNode)
        Argument: TNode;
        Funcname: string;
    end;
    
    // Fine
    TAddition = class(TBinOp);
    TSubtraction = class(TBinOp);
    TMultiplication = class(TBinOp);
    TDivision = class(TBinOp);
    TPower = class(TBinOp);
    TSinus = class(TUnOp);
    TCosinus = class(TUnOp);
    TAbsolute = class(TUnOp);
    TSquareRoot = class(TUnOp);
    
const
    OPF_ADDOP               = $02; // ------X-
    OP_ADDITION             = $02; // ------10
    OP_SUBTRACTION          = $03; // ------11
    OPF_MULOP               = $04; // -----X--
    OP_MULTIPLICATION       = $04; // -----1-0
    OP_DIVISION             = $05; // -----1-1
    OPF_POWER               = $08; // ----X---
    OP_POWER                = $08; // ----1--0
    OP_ROOT                 = $09; // ----1--1
    OPF_UNARY               = $80; // X-------
    OPF_TRIGONOMETRY        = $10; // ---X----
    OP_SINUS                = $90; // 1--1---0
    OP_COSINUS              = $91; // 1--1---1
    OP_SQRT                 = $89; // 1-0-1--1
    OP_CBRT                 = $A9; // 1-1-1--1
    
function HasFlags(const op, mask: byte): boolean; inline;

implementation

function HasFlags(const op, mask: byte): boolean;
begin
    Result := (op and mask)=mask;
end;

end.
