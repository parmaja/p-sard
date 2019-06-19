program sard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  sardClasses, sardLexers, sardOperators, sardObjects, sardRuntimes, sardScanners, sardTypes, sardParsers, sardScripts;

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
  aRun: TSardScript;
  Lines: TStringList;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  aRun := TSardScript.Create;
  try
    try
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(Location + 'test.sard');
        aRun.Compile(Lines);
        aRun.Run;
      finally
        FreeAndNil(Lines);
      end;
    finally
      FreeAndNil(aRun);
    end;
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

