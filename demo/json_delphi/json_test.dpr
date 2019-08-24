program json_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes,
  sardClasses, sardObjects, sardParsers, sardScripts,
  sardJSONs, sardJSONRTTIs;

{.$define DOM}

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

procedure Run;
var
  Scanner: TJSONScanner;
  Lines: TStringList;
  FileName: string;
  {$ifdef DOM}
  JSONRoot: TMyDOM;
  Writer: TStringSourceWriter;
  {$else}
  JSONRoot: TMyJSONObject;
  {$endif}
begin
  try
    if ParamCount > 0 then
    begin
      FileName := ParamStr(1);
      {$ifdef DOM}
      JSONRoot:= TMyDOM.Create;
      Scanner := TJSONScanner.Create(JSONRoot, TDataJSONParser);
      //Scanner.Strict := False;
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
end;

begin
  try
    try
      Run;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    Readln;
  end;
end.
