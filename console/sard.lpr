program sard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  sardClasses, sardObjects, sardParsers, sardScripts, sardJSONs;

type

  { TSardApplication }

  TSardApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    procedure ShowException(E: Exception); override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSardApplication }

procedure TSardApplication.DoRun;
var
  ErrorMsg: String;
  Script: TCodeScript;
  Lines: TStringList;
  FileName: string;
  FileNames: TStringArray;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hw', ['help wait']);
  if ErrorMsg <>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  try
    if ParamCount > 0 then
    begin
      FileNames := GetNonOptions('hw', ['help','wait']);
      if Length(FileNames) > 0 then
        FileName := FileNames[0];
      if (FileName <> '') and FileExists(FileName) then
      begin
        Script := TCodeScript.Create;
        try
          Lines := TStringList.Create;
          try
            Lines.LoadFromFile(FileName);
            //Lines.LoadFromFile(Location + 'test.sard');
            //Lines.Text := 'x:{:=10};';
            //Lines.Text := 'print(10);print(20)';
            Script.RegisterInternals := False;
            Script.Compile(Lines);
            Script.Run;
            Script.ExportToFile('export.sard');
            WriteLn((Script as TCodeScript).Result);
          finally
            FreeAndNil(Lines);
          end;
        finally
          FreeAndNil(Script);
        end;
      end
      else
        WriteLn('File script not exists');
    end;
    {$ifdef DEBUG}
    if HasOption('w', 'wait') then
    begin
      Write('Press enter to exit.');
      ReadLn();
    end;
    {$endif}
  except
    on E:Exception do
    begin
      ShowException(E);
      raise;
    end;
  end;
  Terminate;
end;

procedure TSardApplication.ShowException(E: Exception);
begin
  WriteLn(E.Message);
  inherited ShowException(E);
end;

constructor TSardApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TSardApplication.Destroy;
begin
  inherited;
end;

procedure TSardApplication.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TSardApplication;
begin
  Application := TSardApplication.Create(nil);
  Application.Title :='Sard';
  Application.Run;
  Application.Free;
end.

