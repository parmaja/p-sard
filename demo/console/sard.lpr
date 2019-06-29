program sard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  sardClasses, sardObjects, sardParsers, sardScripts;

type

  { TMyJSONObject }

  TMyJSONObject = class(TPersistent)
  private
    FCaption: string;
    FName: string;
    FTag: Integer;
    FValue: string;
    procedure SetCaption(AValue: string);
    procedure SetName(AValue: string);
    procedure SetTag(AValue: Integer);
    procedure SetValue(AValue: string);
  published
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
    property Caption: string read FCaption write SetCaption;
    property Tag: Integer read FTag write SetTag;
  end;

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

{ TMyJSONObject }

procedure TMyJSONObject.SetCaption(AValue: string);
begin
  if FCaption =AValue then Exit;
  FCaption :=AValue;
end;

procedure TMyJSONObject.SetName(AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

procedure TMyJSONObject.SetTag(AValue: Integer);
begin
  if FTag =AValue then Exit;
  FTag :=AValue;
end;

procedure TMyJSONObject.SetValue(AValue: string);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
end;

{ TSardApplication }

procedure TSardApplication.DoRun;
var
  ErrorMsg: String;
  Script: TCodeScript;
  Lines: TStringList;
  FileName: string;
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

  try
    if ParamCount > 0 then
    begin
      FileName := ParamStr(1);
      Script := TCodeScript.Create;
      try
        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(FileName);
          //Lines.LoadFromFile(Location + 'test.sard');
          //Lines.Text := 'x:{:=10};';
          //Lines.Text := 'print(10);print(20)';
          Script.Compile(Lines);
          if Script is TCodeScript then
          begin
            (Script as TCodeScript).Run;
            WriteLn((Script as TCodeScript).Result);
          end;
        finally
          FreeAndNil(Lines);
        end;
      finally
        FreeAndNil(Script);
      end;
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

