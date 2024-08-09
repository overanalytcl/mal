program fpmake;

{$mode objfpc}{$H+}
uses
   fpmkunit,
   Classes,
   SysUtils;

var
   FileNames: TStringList;

procedure AddExecutable(const AFileName: string);
var
   FileNameWithoutExt: string;
begin
   if FileExists(AFileName) then
   begin
      FileNameWithoutExt := ChangeFileExt(ExtractFileName(AFileName), '');
      FileNames.Add(ExtractFileName(FileNameWithoutExt));
   end;
end;

procedure InitExecutables;
begin
   AddExecutable('step0_repl.pas');
   AddExecutable('step1_read_print.pas');
   AddExecutable('step2_eval.pas');
   AddExecutable('step3_env.pas');
   AddExecutable('step4_if_fn_do.pas');
   AddExecutable('step5_tco.pas');
   AddExecutable('step6_file.pas');
   AddExecutable('step7_quote.pas');
   AddExecutable('step8_macros.pas');
   AddExecutable('step9_try.pas');
   AddExecutable('stepA_mal.pas');
end;

procedure BuildPrograms;
var
   P: TPackage;
   T: TTarget;
   FileName: string;
begin
   FileNames := TStringList.Create;
   try
      InitExecutables;

      with Installer do
      begin
         P := AddPackage('mal');
         P.OSes := [win32, openbsd, netbsd, freebsd, darwin, linux];
         for FileName in FileNames do
         begin
            T := P.Targets.AddUnit(FileName + '.pas');
            T.SetExeName(FileName);
            T.ResourceStrings := True;
         end;
         Run;
      end;
   finally
      FileNames.Free;
   end;
end;

begin
   BuildPrograms;
end.
