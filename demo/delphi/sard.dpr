program sard;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes,
  mnClasses, mnUtils,
  sardClasses, sardObjects, sardParsers, sardScripts;

procedure Run;
var
  Script: TCodeScript;
  Lines: TStringList;
begin
  if ParamCount > 0 then
  begin
    Script := TCodeScript.Create;
    try
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(ParamStr(1));
        //Lines.LoadFromFile(Location + 'test.sard');
        //Lines.Text := 'x:{:=10};';
        //Lines.Text := 'print(10);print(20)';
        Script.Compile(Lines);
        Script.Run;
        WriteLn(Script.Result);
        WriteLn;
      finally
        FreeAndNil(Lines);
      end;
    finally
      FreeAndNil(Script);
    end;
end;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
