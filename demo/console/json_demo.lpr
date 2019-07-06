program json_demo;

{$mode delphi}{$H+}
{.$define DOM}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  sardClasses, sardObjects, sardParsers, sardScripts,
  sardJSONReaders;

type
  {$ifdef DOM}
  TMyDOM = class(TJSONRoot);
  {$else}
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
  {$endif}

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

 {$ifdef DOM}
 {$else}
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
{$endif}

{ TSardApplication }

procedure TSardApplication.DoRun;
var
  ErrorMsg: String;
  i: Integer;
  Scanner: TJSONScanner;
  Lines: TStringList;
  FileName: string;
  Writer: TStringSourceWriter;
  {$ifdef DOM}
  JSONRoot: TMyDOM;
  {$else}
  JSONRoot: TMyJSONObject;
  {$endif}
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
      {$ifdef DOM}
      JSONRoot:= TMyDOM.Create;
      Scanner := TJSONScanner.Create(JSONRoot, TDOMJSONParser);
      {$else}
      JSONRoot:=TMyJSONObject.Create;
      Scanner := TJSONScanner.Create(JSONRoot, TRTTIJSONParser);
      {$endif}
      try
        Lines := TStringList.Create;
        try
          Lines.LoadFromFile(FileName);
          Scanner.Compile(Lines);
          {$ifdef DOM}
          Lines.Clear;
          Writer := TStringSourceWriter.Create(Lines);
          JSONRoot.WriteTo(Writer, True, 0);
          Writer.Free;
          for i := 0 to Lines.Count -1 do
            WriteLn(Lines[i]);
          {$else}
          WriteLn('Name: ', JSONRoot.Name);
          WriteLn('Value: ', JSONRoot.Value);
          WriteLn('Caption: ', JSONRoot.Caption);
          WriteLn('Tag: ', JSONRoot.Tag);
          {$endif}
          FreeAndNil(JSONRoot);
          ReadLn;
        finally
          FreeAndNil(Lines);
        end;
      finally
        FreeAndNil(Scanner);
      end;
    end;
  finally
  end;
  Terminate;
end;

procedure TSardApplication.ShowException(E: Exception);
begin
  inherited ShowException(E);
  Readln();
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
  Application.Title :='JSON Demo';
  Application.Run;
  Application.Free;
end.

