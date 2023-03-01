unit sardJSONs;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey 
*}

{
  bugs:
}

{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  mnUtils, mnClasses,
  sardClasses, sardParsers, sardStandards;

type
  TJSONParser = class;

  { TJSONController }

  TJSONController = class(TController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollector }

  TJSONCollector = class(TCollector)
  private
    function GetParser: TJSONParser;
  protected
    procedure DoControl(AControl: TSardControl); override;
  public
    function IsInitial: Boolean; override;
    procedure Reset; override;
    property Parser: TJSONParser read GetParser;
  end;

  { TJSONCollector_Value }

  TJSONCollector_Value = class(TJSONCollector)
  private
  protected
    type
      TExpect = (valValue, valNext);
    var
      Expect: TExpect;
    CurrentObject: TObject;
    Name: string;
  public
    constructor Create(AParser: TParser; const AName: string; AObject: TObject);
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONCollector_Pair }

  {
    Pair is Name and Value, Value can be String/Object/Array
  }

  TJSONCollector_Pair = class(TJSONCollector)
  private
    Name: string;
  protected
    type
      TExpect = (elmName, elmValue);
    var
      Expect: TExpect;
    ParentPair: TObject;
    CurrentObject: TObject;
  public
    constructor Create(AParser: TParser; AParentPair: TObject; ACurrentObject: TObject = nil); overload;
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONCollector_Array }

  TJSONCollector_Array = class(TJSONCollector)
  private
  protected
    type
      TExpect = (aryValue, aryNext);
    var
      Expect: TExpect;
    ParentPair: TObject; //element with array value created
    Name: string;
  public
    constructor Create(AParser: TParser; const AName: string; AParentObject: TObject);
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONLexer }

  TJSONLexer = class(TLexer)
  protected
    const
      sNumberOpenChars = ['-', '+', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
  public
    constructor Create; override;
    function IsNumber(const vChar: Char; vOpen: Boolean =true): Boolean; override;
    function IsIdentifier(const vChar: Char; vOpen: Boolean =true): Boolean;
  end;

  TJSONType = (
    jtString,
    jtNumber,
    jtIdentifier,
    jtBoolean,
    jtObject,
    jtArray
  );

  { TJSONParser }

  TJSONParser = class abstract(TParser)
  private
    FStrict: Boolean;
  protected
    Lexer: TLexer;
    procedure RequirePair(AParentObject: TObject; const AName: string; out AObject: TObject); virtual;
    function SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject; virtual; abstract;
  public
    constructor Create(ALexer: TLexer; AObject: TObject); overload; virtual;
    destructor Destroy; override;

    property Strict: Boolean read FStrict write FStrict default true;
  end;

  TJSONParserClass = class of TJSONParser;

//-----------------------------------------------------------------------------
//* DOM objects

  { TJSONBase }

  TDOM_JSONBase = class abstract(TSardObject)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); virtual;
  end;

  TDOM_JSONValue = class;

  { TDOM_JSONPair }

  TDOM_JSONPair = class(TDOM_JSONBase)
  private
    FName: string;
    FValue: TDOM_JSONValue;
    procedure SetName(const AValue: string);
    procedure SetValue(AValue: TDOM_JSONValue);
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  published
    property Value: TDOM_JSONValue read FValue write SetValue;
    property Name: string read FName write SetName;
  end;

  { TDOM_JSONRoot }

  TDOM_JSONRoot = class(TDOM_JSONPair)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  end;

  { TDOM_JSONValue }

  TDOM_JSONValue = class abstract(TDOM_JSONBase)
  private
    FParent: TDOM_JSONPair;
  public
    constructor Create(AParent: TDOM_JSONPair);
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Parent: TDOM_JSONPair read FParent;
  end;

  TDOM_JSONValueClass = class of TDOM_JSONValue;

  { TDOM_JSONString_Value }

  TDOM_JSONString_Value = class(TDOM_JSONValue)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    constructor Create(AParent: TDOM_JSONPair; const AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TDOM_JSONIdentifier_Value }

  TDOM_JSONIdentifier_Value = class(TDOM_JSONValue)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    constructor Create(AParent: TDOM_JSONPair; const AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TDOM_JSONNumber_Value }

  TDOM_JSONNumber_Value = class(TDOM_JSONValue)
  private
    FNumber: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TDOM_JSONPair; const ANumber: string); overload;
  published
    property Number: string read FNumber write FNumber;
  end;

  { TDOM_JSONBoolean_Value }

  TDOM_JSONBoolean_Value = class(TDOM_JSONValue)
  private
    FValue: Boolean;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TDOM_JSONPair; AValue: Boolean); overload;
    constructor Create(AParent: TDOM_JSONPair; const AValue: string); overload;
  published
    property Value: Boolean read FValue write FValue;
  end;

  { TDOM_JSONItems }

  TDOM_JSONItems = class(TmnObjectList<TDOM_JSONPair>)
  public
  end;

  { TDOM_JSONObject_Value }

  TDOM_JSONObject_Value = class(TDOM_JSONValue)
  private
    FItems: TDOM_JSONItems;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure RequirePair(const AJSONName: string; out AJSONObject: TObject);
    procedure Add(Value: TDOM_JSONPair); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Items: TDOM_JSONItems read FItems;
  published
  end;

  { TDOM_JSONList }

  TDOM_JSONList = class(TmnObjectList<TDOM_JSONValue>)
  public
  end;

  { TDOM_JSONArray_Value }

  TDOM_JSONArray_Value = class(TDOM_JSONValue)
  private
    FItems: TDOM_JSONList;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure Add(Value: TDOM_JSONValue); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    property Items: TDOM_JSONList read FItems;
  published
  end;

//-----------------------------------------------------------------------------

 { TDataJSONParser }

  TDataJSONParser = class(TJSONParser)
  protected
  protected
    procedure RequirePair(AParentObject: TObject; const AJSONName: string; out AJSONObject: TObject); override;
    function SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject; override;
  public
  end;

  { TJSONScanner }

  TJSONScanner = class(TScanner)
  private
    FStrict: Boolean;
  protected
    FParserClass: TJSONParserClass;
    FRoot: TObject;
    function CreateParser: TParser; override;
  public
    constructor Create(ARoot: TObject; AParserClass: TJSONParserClass);
    procedure Compile(Lines: TStringList); overload;
    procedure Compile(const Text: string); overload;
    procedure CompileStream(const Stream: TStream); overload;
    procedure CompileFile(const FileName: string); overload;
    property Root: TObject read FRoot;
    property Strict: Boolean read FStrict write FStrict default true;
  end;

implementation

uses
  StrUtils;

{ TDOM_JSONBoolean_Value }

procedure TDOM_JSONBoolean_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Value then
    Writer.Add('true')
  else
    Writer.Add('false');
  inherited WriteTo(Writer, LastOne, Level);
end;

constructor TDOM_JSONBoolean_Value.Create(AParent: TDOM_JSONPair; AValue: Boolean);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

constructor TDOM_JSONBoolean_Value.Create(AParent: TDOM_JSONPair; const AValue: string);
begin
  inherited Create(AParent);
  if AValue = 'true' then
    FValue := true
  else if AValue = 'false' then
    FValue := false;
end;

{ TDOM_JSONIdentifier_Value }

procedure TDOM_JSONIdentifier_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Text);
  inherited;
end;

constructor TDOM_JSONIdentifier_Value.Create(AParent: TDOM_JSONPair; const AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

{ TJSONCollector_Array }

constructor TJSONCollector_Array.Create(AParser: TParser; const AName: string; AParentObject: TObject);
begin
  inherited Create(AParser);
  Name := AName;
  ParentPair := AParentObject;
end;

procedure TJSONCollector_Array.Reset;
begin
  inherited Reset;
  Expect := Low(Expect);
end;

procedure TJSONCollector_Array.DoToken(Token: TSardToken);
begin
  RaiseError('No token for array!');
end;

procedure TJSONCollector_Array.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlOpenArray:
    begin
      Parser.Push(TJSONCollector_Value.Create(Parser, Name, ParentPair));
    end;
    ctlCloseArray:
    begin
      Parser.SetAction([paPop]);
      Reset;
    end;
    ctlNext:
    begin
      Parser.Push(TJSONCollector_Value.Create(Parser, Name, ParentPair));
      Reset;
    end;
    else
      inherited;
  end;
end;

{ TDOM_JSONRoot }

procedure TDOM_JSONRoot.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Value.WriteTo(Writer, LastOne, Level);
end;

{ TDOM_JSONNumber_Value }

procedure TDOM_JSONNumber_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Number);
  inherited;
end;

constructor TDOM_JSONNumber_Value.Create(AParent: TDOM_JSONPair; const ANumber: string);
begin
  inherited Create(AParent);
  FNumber := ANumber;
end;

{ TDOM_JSONValue }

constructor TDOM_JSONValue.Create(AParent: TDOM_JSONPair);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TDOM_JSONValue.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Self = nil then
    Writer.Add('null');
  if not LastOne then
    Writer.Add(',');
  Writer.NewLine;
end;

{ TDOM_JSONBase }

procedure TDOM_JSONBase.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin

end;

{ TDOM_JSONString_Value }

procedure TDOM_JSONString_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(QuoteStr(EscapeStringC(Text), '"'));
  inherited;
end;

constructor TDOM_JSONString_Value.Create(AParent: TDOM_JSONPair; const AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

{ TDOM_JSONArray_Value }

procedure TDOM_JSONArray_Value.Created;
begin
  inherited;
  FItems := TDOM_JSONList.Create;
end;

destructor TDOM_JSONArray_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TDOM_JSONArray_Value.Add(Value: TDOM_JSONValue);
begin
  Items.Add(Value);
end;

procedure TDOM_JSONArray_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TDOM_JSONValue;
begin
  Writer.Add('[');
  Writer.NewLine;
  for Itm in Items do
  begin
    Writer.Add(Level + 1);
    Itm.WriteTo(Writer, itm = Items.Last , Level + 1);
  end;
  Writer.Add(Level, ']');
  inherited;
end;

{ TDOM_JSONObject_Value }

procedure TDOM_JSONObject_Value.Created;
begin
  inherited;
  FItems := TDOM_JSONItems.Create;
end;

destructor TDOM_JSONObject_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TDOM_JSONObject_Value.RequirePair(const AJSONName: string; out AJSONObject: TObject);
begin
  AJSONObject := TDOM_JSONPair.Create;
  (AJSONObject as TDOM_JSONPair).Name := AJSONName;
  Add((AJSONObject as TDOM_JSONPair));
end;

procedure TDOM_JSONObject_Value.Add(Value: TDOM_JSONPair);
begin
  Items.Add(Value);
end;

procedure TDOM_JSONObject_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TDOM_JSONPair;
begin
  Writer.Add('{');
  Writer.NewLine;
  for Itm in Items do
    Itm.WriteTo(Writer, itm = Items.Last , Level + 1);
  Writer.Add(Level, '}');
  inherited;
end;

{ TDOM_JSONPair }

procedure TDOM_JSONPair.SetValue(AValue: TDOM_JSONValue);
begin
  if FValue <> AValue then
  begin
    if (AValue.Parent <> nil) and (AValue.Parent <> self) then
      RaiseError('Value have parent we can`t move it to another parent');
    FreeAndNil(FValue);
    FValue := AValue;
    FValue.FParent := Self;
  end;
end;

procedure TDOM_JSONPair.SetName(const AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

procedure TDOM_JSONPair.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Level, QuoteStr(Name, '"') + ': ');
  if Value = nil then
    RaiseError('Value is null for: ' + Name)
  else
    Value.WriteTo(Writer, LastOne, Level);
end;

{ TDataJSONParser }

procedure TDataJSONParser.RequirePair(AParentObject: TObject; const AJSONName: string; out AJSONObject: TObject);
begin
  if not (AParentObject is TDOM_JSONObject_Value) then
    RaiseError('Value is not object');
  (AParentObject as TDOM_JSONObject_Value).RequirePair(AJSONName, AJSONObject);
end;

function TDataJSONParser.SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject;
var
  v: TDOM_JSONValue;
  procedure CreateValue;
  begin
    case AType of
      jtNumber: v :=  TDOM_JSONNumber_Value.Create(nil, AValue);
      jtIdentifier: v :=  TDOM_JSONIdentifier_Value.Create(nil, AValue);
      jtBoolean: v :=  TDOM_JSONBoolean_Value.Create(nil, AValue);
      jtString: v :=  TDOM_JSONString_Value.Create(nil, AValue);
      jtObject: v := TDOM_JSONObject_Value.Create(nil);
      jtArray: v := TDOM_JSONArray_Value.Create(nil);
    end;
    Result := v;
  end;
begin
  if AObject = nil then
    RaiseError('Can not set value to nil object');

  if (AObject is TDOM_JSONArray_Value) then
  begin
    CreateValue;
    (AObject as TDOM_JSONArray_Value).Add(v);
  end
  else if (AObject is TDOM_JSONPair) then
  begin
     if (AObject as TDOM_JSONPair).Value <> nil then
      RaiseError('Value is already set and it is not array: ' + AObject.ClassName);
    CreateValue;
    (AObject as TDOM_JSONPair).Value  :=  v;
  end
  else
    RaiseError('Value can not set to:' + AObject.ClassName);
end;

{ TJSONCollector_Value }

constructor TJSONCollector_Value.Create(AParser: TParser; const AName: string; AObject: TObject);
begin
  inherited Create(AParser);
  if AObject = nil then
    RaiseError('Object nil in collection value!');
  Name := AName;
  CurrentObject := AObject;
end;

procedure TJSONCollector_Value.Reset;
begin
  inherited Reset;
  Expect := Low(Expect);
end;

procedure TJSONCollector_Value.DoToken(Token: TSardToken);
begin
  if Expect = valValue then
  begin
    if (Token.TokenType = typeString) then
      Parser.SetObjectValue(CurrentObject, Name, Token.Value, jtString)
    else if (Token.TokenType = typeIdentifier) then
    begin
      if (Token.Value = 'true') or (Token.Value = 'false') then
        Parser.SetObjectValue(CurrentObject, Name, Token.Value, jtBoolean)
      else
        Parser.SetObjectValue(CurrentObject, Name, Token.Value, jtIdentifier)
    end
    else if (Token.TokenType = typeNumber) then
      Parser.SetObjectValue(CurrentObject, Name, Token.Value, jtNumber);
    Inc(Expect);
  end
  else
    RaiseError('Value not expected: ' + Token.Value);
end;

procedure TJSONCollector_Value.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlOpenBlock:
    begin
      if Expect = valValue then
      begin
        Parser.Push(TJSONCollector_Pair.Create(Parser, CurrentObject, Parser.SetObjectValue(CurrentObject, Name, '', jtObject)));
        Inc(Expect);
      end
      else
        inherited;
    end;
    ctlCloseBlock:
    begin
      if (Expect = valNext) or not Parser.Strict then
      begin
        Parser.SetAction([paPass, paPop]);
      end
      else
        inherited;
    end;
    ctlOpenArray:
    begin
      if Expect = valValue then
      begin
        Parser.Push(TJSONCollector_Array.Create(Parser, Name, Parser.SetObjectValue(CurrentObject, Name, '', jtArray)));
        Parser.SetAction([paPass]);
        Inc(Expect);
      end
      else
        inherited;
    end;
    ctlCloseArray:
    begin
      Parser.SetAction([paPass, paPop]);//pass it to parent
    end;
    ctlStop: //ctlStop using stop if file ended after value, we treat it as comma, but in Element collector we will check if that error
    begin
      if (Expect = valNext) or not Parser.Strict then
      begin
        Parser.SetAction([paPass, paPop]);
        Reset;
      end
      else
        inherited;
    end;
    ctlNext:
    begin
      if (Expect = valNext) or not Parser.Strict then
      begin
        Parser.SetAction([paPass, paPop]); //paPass pass it to array to push another value collection, but element should ignore it
        Reset;
      end
      else
        inherited;
    end;
    else
      inherited;
  end;
end;

{ TJSONCollector_Pair }

constructor TJSONCollector_Pair.Create(AParser: TParser; AParentPair: TObject; ACurrentObject: TObject);
begin
  inherited Create(AParser);
  ParentPair := AParentPair;
  CurrentObject := ACurrentObject;
end;

procedure TJSONCollector_Pair.Reset;
begin
  inherited Reset;
  Name := '';
  Expect := Low(Expect);
end;

procedure TJSONCollector_Pair.DoToken(Token: TSardToken);
begin
  if (Expect = elmName) and ((Token.TokenType = typeIdentifier) or (Token.TokenType = typeString)) then
  begin
    if Name <> '' then
      RaiseError('Name already set: ' + Name);
    Name := Token.Value;
    //Inc(Expect);
  end
  else
    RaiseError('Name expected');
end;

procedure TJSONCollector_Pair.DoControl(AControl: TSardControl);
var
  AObject: TObject;
begin
  case AControl.Code of
    ctlStart:
    begin
      if Expect = elmName then
      begin
        Parser.Push(TJSONCollector_Value.Create(Parser, Name, ParentPair));
      end
      else
        inherited;
    end;

    ctlAssign:
    begin
      if Expect = elmName then
      begin
        Parser.RequirePair(CurrentObject, Name, AObject);
        Parser.Push(TJSONCollector_Value.Create(Parser, Name, AObject));
        Inc(Expect);//To Value
      end
      else
        inherited;
    end;

    ctlCloseBlock:
    begin
      if (Expect = elmValue) or not Parser.Strict then
      begin
        Parser.SetAction([paPop]);
        Reset;
      end
      else
        inherited;
    end;

    ctlNext:
    begin
      if Expect = elmValue then
        Reset //good if it comes from values of array
      else
        inherited;
    end;
    else
      inherited;
  end;
end;

{ TJSONCollector }

function TJSONCollector.GetParser: TJSONParser;
begin
  Result := (inherited Parser) as TJSONParser;
end;

procedure TJSONCollector.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlStart: ;
    ctlStop:
    begin
      if (Parser.Count > 1) and Parser.Strict then
        RaiseError('Incomplete Code');
    end
    else
      RaiseError('Not Expected ' + AControl.Name);
  end;
end;

function TJSONCollector.IsInitial: Boolean;
begin
  Result := false;
end;

procedure TJSONCollector.Reset;
begin
end;

procedure TJSONController.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollector) do
  begin
  end;
end;

{ TJSONScanner }

function TJSONScanner.CreateParser: TParser;
begin
  Result := FParserClass.Create(Current, FRoot);
  (Result as TJSONParser).Strict := Strict;
end;

procedure TJSONScanner.Compile(Lines: TStringList);
begin
  Scan(Lines);
end;

procedure TJSONScanner.Compile(const Text: string);
begin
  Scan(Text);
end;

procedure TJSONScanner.CompileFile(const FileName: string);
begin
  ScanFile(FileName);
end;

procedure TJSONScanner.CompileStream(const Stream: TStream);
begin
  Scan(Stream);
end;

constructor TJSONScanner.Create(ARoot: TObject; AParserClass: TJSONParserClass);
begin
  inherited Create;
  FStrict := True;
  FRoot := ARoot;
  FParserClass := AParserClass;
  Add(TJSONLexer.Create);
end;

{ TJSONParser }

procedure TJSONParser.RequirePair(AParentObject: TObject; const AName: string; out AObject: TObject);
begin
  AObject := nil;
end;

constructor TJSONParser.Create(ALexer: TLexer; AObject: TObject);
begin
  inherited Create(False); //TODO check if own
  FStrict := True;
  Lexer := ALexer;
  Push(TJSONCollector_Pair.Create(Self, AObject));
end;

destructor TJSONParser.Destroy;
begin
  inherited;
end;

{ TJSONLexer }

constructor TJSONLexer.Create;
begin
  inherited;

  Add(TWhitespace_Tokenizer.Create);
  Add(TNumber_Tokenizer.Create);
  Add(TSL_DQ_String_Tokenizer.Create);
  //Add(TML_Comment_Tokenizer.Create);
  //Add(TSL_Comment_Tokenizer.Create);
  Add(TControl_Tokenizer.Create('(', ctlOpenParams));
  Add(TControl_Tokenizer.Create('[', ctlOpenArray));
  Add(TControl_Tokenizer.Create('{', ctlOpenBlock));
  Add(TControl_Tokenizer.Create(')', ctlCloseParams));
  Add(TControl_Tokenizer.Create(']', ctlCloseArray));
  Add(TControl_Tokenizer.Create('}', ctlCloseBlock));
  Add(TControl_Tokenizer.Create(',', ctlNext));
  Add(TControl_Tokenizer.Create(':', ctlAssign));

  Add(TIdentifier_Tokenizer.Create);//Sould be last one
end;

function TJSONLexer.IsNumber(const vChar: Char; vOpen: Boolean): Boolean;
begin
  if (vOpen) then
    Result := CharInSet(vChar, sNumberOpenChars)
  else
    Result := CharInSet(vChar, sNumberChars);
end;

function TJSONLexer.IsIdentifier(const vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := inherited isIdentifier(vChar, vOpen); //we do not need to override it, but it is nice to see it here
end;

end.
