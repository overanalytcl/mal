program fpmake;

{$mode objfpc}{$H+}

uses
   fpmkunit,
   Classes,
   SysUtils,
   Generics.Collections;

type
   TDependenciesMap = specialize TDictionary<string, TStringList>;

var
   FileNames: TStringList;
   DependenciesMap: TDependenciesMap;

procedure AddExecutable(const AFileName: string; const Dependencies: array of string);
var
   FileNameWithoutExt: string;
   DependencyList: TStringList;
   Dependency: string;
begin
   if FileExists(AFileName) then
   begin
      FileNameWithoutExt := ChangeFileExt(ExtractFileName(AFileName), '');
      FileNames.Add(FileNameWithoutExt);

      if not DependenciesMap.TryGetValue(FileNameWithoutExt, DependencyList) then
      begin
         DependencyList := TStringList.Create;
         DependenciesMap.Add(FileNameWithoutExt, DependencyList);
      end;

      for Dependency in Dependencies do
      begin
         if FileExists(Dependency) then
         begin
            DependencyList.Add(ChangeFileExt(ExtractFileName(Dependency), ''));
         end
         else
         begin
            WriteLn(Format('Warning: Dependency file "%s" does not exist.',
               [Dependency]));
         end;
      end;
   end;
end;

procedure InitExecutables;
begin
   AddExecutable('editline.pas', []);

   AddExecutable('step0_repl.pas', ['editline.pas']);
   AddExecutable('step1_read_print.pas', ['editline.pas']);
   AddExecutable('step2_eval.pas', ['editline.pas']);
   AddExecutable('step3_env.pas', ['editline.pas']);
   AddExecutable('step4_if_fn_do.pas', ['editline.pas']);
   AddExecutable('step5_tco.pas', ['editline.pas']);
   AddExecutable('step6_file.pas', ['editline.pas']);
   AddExecutable('step7_quote.pas', ['editline.pas']);
   AddExecutable('step8_macros.pas', ['editline.pas']);
   AddExecutable('step9_try.pas', ['editline.pas']);
   AddExecutable('stepA_mal.pas', ['editline.pas']);
end;

procedure BuildPrograms;
var
   P: TPackage;
   T: TTarget;
   FileName, Dependency: string;
   Dependencies: TStringList;
begin
   FileNames := TStringList.Create;
   DependenciesMap := TDependenciesMap.Create;
   try
      InitExecutables;

      with Installer do
      begin
         P := AddPackage('mal');
         P.OSes := [win32, openbsd, netbsd, freebsd, darwin, linux];
         for FileName in FileNames do
         begin
            T := P.Targets.AddUnit(FileName + '.pas');
            T.Options.AddStrings(['-O3', '-Si', '-XX']);
            T.SetExeName(FileName);
            T.ResourceStrings := True;

            if DependenciesMap.TryGetValue(FileName, Dependencies) then
            begin
               for Dependency in Dependencies do
               begin
                  T.Dependencies.AddUnit(Dependency);
               end;
            end;
         end;
         Run;
      end;
   finally
      DependenciesMap.Free;
      FileNames.Free;
   end;
end;

begin
   BuildPrograms;
end.
