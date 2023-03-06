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
  sardStandards,
  sardScripts,
  sardJSONs,
  sardJSONRTTIs,
  mnConfigs,
  mnDON, mnJSON,
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

{$define DON}

  {$ifdef DON}
  {$else}
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
  {$endif}

 {$ifdef DON}
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
  {$ifdef DON}
  i: Integer;
  JSONRoot: TDON_Pair;
  Writer: TStringsSerializer;
  {$else}
  JSONRoot: TMyJSONObject;
  {$endif}
begin
  DebugMode := True;
  try
//    if ParamCount > 0 then
    begin
      FileName := ExtractFilePath(ParamStr(0))+ 'data.json';// ParamStr(1);
      try
        Lines := TStringList.Create;
        try
          WriteLn('Loading: ' + FileName);
          Lines.LoadFromFile(FileName);
          var s := Lines.Text;

          LogBeginTick;
          var Json2 := JsonDataObjects.TJSONObject.Parse(s);
          LogEndTick('JSONObject');
          Json2.Free;

          LogBeginTick;
          var Json1 := Json.TJSONObject.ParseJsonValue(s, False, True);
          LogEndTick('Delphi JSON');
          Json1.Free;

          LogBeginTick;
          var Json3 := JsonParseStringValue(s);
          LogEndTick('mnJSON');
          Json3.Free;
{
          var v := Json3.AsString;
          var v := Json3['"books.zaher'].AsString;
          var v := Json3['books']['zaher'].AsString;
}


          var Json4 := JsonParseFileValue('test.json', []);
          Writeln(Json4['Books']['Library'].AsString);
          Writeln(Json4['Books']['Book1']['Title'].AsString);
          Writeln(Json4.ByPath('Books.Book1.Title').AsString);
          Writeln(Json4.ByPath('Books\Book1\Title', '\').AsString);
          Writeln(Json4.ByPath(['Books','Book1','Title']).AsString);

          Json4.ByPath(['Books']).AddObject('Book2');
          Json4.ByPath(['Books', 'Book2']).AddPair('Title', 'No one care');
          Json4.ByPath(['Books', 'Book2']).AddPair('ISPN', '545454610');
//          Json4.ByPath(['Books', 'Book2']).Let('Pages', '10');
          //Json4.ByPath(['Books','Book1']).Add('Ventors', ['10']);

          Lines.Clear;

          Writer := TStringsSerializer.Create(Lines);
          JSon4.Serialize(Writer, True, 0);
          Writer.Free;
          for i := 0 to Lines.Count -1 do
            WriteLn(Lines[i]);

          Json4.Free;


          (*
          {$ifdef DON}
          JSONRoot:= TMyDON.Create;
          Scanner := TJSONScanner.Create(JSONRoot, TDataJSONParser);
          //Scanner.Strict := False;
          {$else}
          JSONRoot:=TMyJSONObject.Create;
          Scanner := TJSONScanner.Create(JSONRoot, TRTTIJSONParser);
          {$endif}

          LogBeginTick;
          Scanner.Compile(Lines);
          //Scanner.CompileFile(FileName);
          //Scanner.Compile(s);
          LogEndTick('SardJSON');
          FreeAndNil(JSONRoot);
          FreeAndNil(Scanner);
          *)

          {$ifdef DON}
          //WriteLn('Name: ', JSONRoot.Name);
          Lines.Clear;

{          Writer := TStringsSerializer.Create(Lines);
          JSONRoot.WriteTo(Writer, True, 0);
          Writer.Free;
          for i := 0 to Lines.Count -1 do
            WriteLn(Lines[i]);}

          {$else}
          WriteLn('Name: ', JSONRoot.Name);
          WriteLn('Value: ', JSONRoot.Value);
          WriteLn('Caption: ', JSONRoot.Caption);
          WriteLn('Tag: ', JSONRoot.Tag);
          {$endif}

        finally
          FreeAndNil(Lines);
        end;
      finally
      end;
    end;
  finally
  end;
end;

begin
  Application := TObject.Create;
  //HookCode(@TObject.NewInstance, @HookedObjectNewInstance);
  //HookCode(@TObject.FreeInstance, @HookedObjectFreeInstance);
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

  //TFile.Delete('object.txt');
  //TFile.AppendAllText('object.txt', S);
  Application.Free;
end.

