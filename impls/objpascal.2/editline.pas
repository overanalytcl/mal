unit editline;

{$mode objfpc}{$H+}

interface

uses
   SysUtils, Classes;

type
   EEditlineEOF = class(Exception);

   { TEditLine }

   TEditLine = class
   private
   class var FHistory: TStringList;
   class var FHistorySize: integer;
   class var FDefaultPrompt: string;
   public
      class procedure Initialize;
      class procedure Finalize;
      class function ReadLine(const Prompt: string = ''): string;
      class procedure AddToHistory(const Line: string);
      class procedure LoadHistory(const FileName: string);
      class procedure SaveHistory(const FileName: string);
      class procedure SetHistorySize(Size: integer);
      class procedure SetDefaultPrompt(const Prompt: string);
   end;

implementation

{$LinkLib libedit}

function _Readline(Prompt: PChar): PChar; cdecl; external Name 'readline';
procedure _AddHistory(Line: PChar); cdecl; external Name 'add_history';

{ TEditLine }

class procedure TEditLine.Initialize;
begin
   FHistory := TStringList.Create;
   FHistorySize := 1024;
end;

class procedure TEditLine.Finalize;
begin
   FHistory.Free;
end;

class function TEditLine.ReadLine(const Prompt: string): string;
var
   Line: PChar;
   EffectivePrompt: string;
begin
   EffectivePrompt := Prompt;
   if EffectivePrompt = '' then
      EffectivePrompt := FDefaultPrompt;

   Line := _Readline(PChar(EffectivePrompt));
   try
      if Line = nil then
         raise EEditlineEOF.Create('End of input (EOF) reached.');

      Result := StrPas(Line);

      if not Result.IsEmpty then
         AddToHistory(Result);
   finally
      StrDispose(Line);
   end;
end;

class procedure TEditLine.AddToHistory(const Line: string);
begin
   _AddHistory(PChar(Line));
   FHistory.Add(Line);
   while FHistory.Count > FHistorySize do
      FHistory.Delete(0);
end;

class procedure TEditLine.LoadHistory(const FileName: string);
begin
   if FileExists(FileName) then
   begin
      try
         FHistory.LoadFromFile(FileName);
      except
         on E: Exception do
            raise Exception.CreateFmt('Failed to load history from file "%s": %s',
               [FileName, E.Message]);
      end;
   end;
end;

class procedure TEditLine.SaveHistory(const FileName: string);
begin
   try
      FHistory.SaveToFile(FileName);
   except
      on E: Exception do
         raise Exception.CreateFmt('Failed to save history to file "%s": %s',
            [FileName, E.Message]);
   end;
end;

class procedure TEditLine.SetHistorySize(Size: integer);
begin
   if Size < 0 then
      raise Exception.Create('History size cannot be negative');
   FHistorySize := Size;
end;

class procedure TEditLine.SetDefaultPrompt(const Prompt: string);
begin
   FDefaultPrompt := Prompt;
end;


initialization
   begin
      TEditLine.FHistory := TStringList.Create;
      TEditLine.FHistorySize := 1024;
   end;

finalization
   TEditLine.FHistory.Free;

end.
