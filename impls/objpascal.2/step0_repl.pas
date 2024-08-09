program Step0_REPL;

{$mode objfpc}{$H+}

uses
   editline,
   SysUtils;

function Read(const Str: string): string;
begin
   Result := Str;
end;

function Eval(Ast: string; Env: string): string;
begin
   Result := Ast;
end;

function Print(Exp: string): string;
begin
   Result := Exp;
end;

function Rep(Str: string; ReplEnv: string = ''): string;
begin
   Result := Print(Eval(Read(Str), ReplEnv));
end;

var
   Line: string;
begin
   TEditLine.SetDefaultPrompt('user> ');

   while True do
   begin
      try
         Line := TEditLine.ReadLine();
         if Line = '' then continue;
         WriteLn(Rep(Line));
      except
         on E: EEditlineEOF do Exit;
      end;
   end;

end.
