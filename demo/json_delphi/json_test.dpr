program json_test;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Windows,
  Classes,
  Json,
  ioUtils,
  mnUtils,
  prmClasses,
  sardClasses,
  sardObjects,
  sardParsers,
  sardScripts,
  sardJSONs,
  sardJSONRTTIs,
  JsonDataObjects in '..\..\..\..\JsonDataObjects\Source\JsonDataObjects.pas';

var
  Application: TObject;
  FObjCount: Integer = 0;
  s: string = '';

function HookCode(const ACodeAddress, AHookAddress: Pointer): Boolean;
const
  SIZE_OF_JUMP = 5;
  JMP_RELATIVE = $E9;

var
  OldProtect: DWORD;
  P: PByte;
  Displacement: Integer;
begin
  Result := VirtualProtect(ACodeAddress, SIZE_OF_JUMP,
    PAGE_EXECUTE_READWRITE, OldProtect);

  if (Result) then
  begin
    P := ACodeAddress;
    P^ := JMP_RELATIVE;
    Inc(P);

    Displacement := UIntPtr(AHookAddress) -
      (UIntPtr(ACodeAddress) + SIZE_OF_JUMP);
    PInteger(P)^ := Displacement;

    VirtualProtect(ACodeAddress, SIZE_OF_JUMP, OldProtect, OldProtect);
  end;
end;

procedure HookedObjectFreeInstance(const Self: TObject);
begin
  Dec(FObjCount);

  Self.CleanupInstance;
  FreeMem(Pointer(Self));
end;

function HookedObjectNewInstance(const Self: TClass): TObject;
var
  Instance: Pointer;
begin
  GetMem(Instance, Self.InstanceSize);
  Result := Self.InitInstance(Instance);
  {$IFDEF AUTOREFCOUNT}
  TObjectOpener(Result).FRefCount := 1;
  {$ENDIF}

  Inc(FObjCount);

  System.TMonitor.Enter(Application);
  try
    s := s + #13 + Result.ClassName;
  finally
    System.TMonitor.Exit(Application);
  end;
end;

{$define DOM}

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
  i: Integer;
  JSONRoot: TMyDOM;
  Writer: TStringSourceWriter;
  {$else}
  JSONRoot: TMyJSONObject;
  {$endif}
begin
  DebugMode := True;
  try
//    if ParamCount > 0 then
    begin
      FileName := ExtractFilePath(ParamStr(0))+ 'test.json';// ParamStr(1);
      try
        Lines := TStringList.Create;
        try
          WriteLn('Loading: ' + FileName);
          Lines.LoadFromFile(FileName);
          var s := Lines.Text;
          LogBeginTick;
          var Json := Json.TJSONObject.ParseJsonValue(s);
          LogEndTick('Delphi JSON');
          Json.Free;


          LogBeginTick;
          var Json2 := JsonDataObjects.TJSONObject.Parse(s);
          LogEndTick('JSONObject');
          Json2.Free;

          {$ifdef DOM}
          JSONRoot:= TMyDOM.Create;
          Scanner := TJSONScanner.Create(JSONRoot, TDataJSONParser);
          //Scanner.Strict := False;
          {$else}
          JSONRoot:=TMyJSONObject.Create;
          Scanner := TJSONScanner.Create(JSONRoot, TRTTIJSONParser);
          {$endif}

          LogBeginTick;
          Scanner.Compile(S);
          LogEndTick('SardJSON');
          {$ifdef DOM}
          //WriteLn('Name: ', JSONRoot.Name);
          Lines.Clear;
             {
          Writer := TStringSourceWriter.Create(Lines);
          JSONRoot.WriteTo(Writer, True, 0);
          Writer.Free;
          for i := 0 to Lines.Count -1 do
            WriteLn(Lines[i]);
            }

          {$else}
          WriteLn('Name: ', JSONRoot.Name);
          WriteLn('Value: ', JSONRoot.Value);
          WriteLn('Caption: ', JSONRoot.Caption);
          WriteLn('Tag: ', JSONRoot.Tag);
          {$endif}
          FreeAndNil(JSONRoot);
          //ReadLn;
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
  Application := TObject.Create;
  HookCode(@TObject.NewInstance, @HookedObjectNewInstance);
  HookCode(@TObject.FreeInstance, @HookedObjectFreeInstance);
  try
    try
      Run;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    WriteLn('Press Enter to exit');
    Readln;
  end;
  TFile.AppendAllText('object.txt', S);
  Application.Free;
end.
