program sard_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Classes,
  Forms, main, sard, sardScripts, sardObjects;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.BidiMode := bdLeftToRight;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

