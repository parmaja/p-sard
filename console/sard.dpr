program Sard;

{$apptype CONSOLE}

uses
  Classes, SysUtils,
  sardClasses, sardObjects, sardParsers, sardScripts,
  NodeConsole, sardJSONs;

{$R *.res}

var
  Application: TSardConsole;

begin
  Application := TSardConsole.Create;
  Application.Run;
  if Application.WaitKey then
  begin
    Write('Press Enter to exit.');
    Readln;
  end;
  Application.Free;
end.
