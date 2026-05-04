program Sard;

{$mode objfpc}{$H+}
{$apptype CONSOLE}

uses
  Crt,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  sardClasses, sardObjects, sardParsers, sardScripts,
  SardConsole;

{$R *.res}

var
  Application: TSardConsole;

begin
  Application := TSardConsole.Create;
  Application.Run;
  if Application.WaitKey then
  begin
    Write('Press Enter to exit.');
    KeyPressed;
  end;
  Application.Free;
end.

