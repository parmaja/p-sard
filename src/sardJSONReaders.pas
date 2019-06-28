unit sardJSONReaders;
{**
*  This file is part of the "SARD"
*
* @license   The MIT License (MIT)
*            Included in this distribution
* @author    Zaher Dirkey <zaher at parmaja dot com>
*}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils, TypInfo, Variants,
  mnUtils, mnClasses,
  sardClasses, sardParsers, sardStandards;

type
  TJSONParser = class;

  { TControllerNormal }

  TControllerNormal = class(TController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollector }

  TJSONCollector = class(TCollector)
  private
  public
    function IsInitial: Boolean; override;
    procedure Reset; override;
    procedure Prepare; override;
    procedure Post; override;
    procedure Next; override;
  end;

  { TJSONControllerName }

  TJSONControllerName = class(TController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollectorName }

  TJSONCollectorName = class(TJSONCollector)
  private
  protected
    JSONName: string;
    JSONObject: TObject;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; AJSONObject: TObject);
    procedure Prepare; override;
    procedure Post; override;
    procedure AddToken(Token: TSardToken); override;
  end;

  { TJSONControllerValue }

  TJSONControllerValue = class(TController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollectorValue }

  TJSONCollectorValue = class(TJSONCollector)
  private
  protected
    JSONName: string;
    JSONObject: TObject;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; AName: string; AJSONObject: TObject);
    procedure Prepare; override;
    procedure AddToken(Token: TSardToken); override;
  end;

  { TJSONLexer }

  TJSONLexer = class(TLexer)
  protected
    const
      sWhitespace = sEOL + [' ', #8];
      sNumberOpenChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
      sSymbolChars = ['"', '''', '\']; //deprecated;
      sIdentifierSeparator = '.';
  public
    constructor Create; override;
    function IsEOL(vChar: Char): Boolean; override;
    function IsWhiteSpace(vChar: char; vOpen: Boolean =true): Boolean; override;
    function IsControl(vChar: Char): Boolean; override;
    function IsOperator(vChar: Char): Boolean; override;
    function IsNumber(vChar: Char; vOpen: Boolean =true): Boolean; override;
    function IsSymbol(vChar: Char): Boolean; override;
    function IsIdentifier(vChar: Char; vOpen: Boolean =true): Boolean;
  end;

  { TJSONScanner }

  TJSONScanner = class(TScanner)
  protected
    Parser: TJSONParser;
    FRoot: TObject;
    function CreateParser: TParser; override;
  public
    constructor Create(ARoot: TObject);
    property Root: TObject read FRoot;
  end;

  { TJSONParser }

  TJSONParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    procedure DoQueue;
    procedure SetObject(AJSONObject: TObject; AJSONName: string; AJSONValue: string); virtual;
  public
    constructor Create(ALexer: TLexer; AJSONObject: TObject);
    destructor Destroy; override;

    procedure SetToken(Token: TSardToken); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  { TRTTIJSONParser }

  TRTTIJSONParser = class(TJSONParser)
  protected
    procedure SetObject(AJSONObject: TObject; AJSONName: string; AJSONValue: string); override;
  end;

  { TDOMJSONParser }

  TDOMJSONParser = class(TJSONParser)
  protected
    procedure SetObject(AJSONObject: TObject; AJSONName: string; AJSONValue: string); override;
  end;

  { TJSONScript }

  TJSONScript = class(TScript)
  public
    FRoot: TObject;
    Scanner: TJSONScanner;
    constructor Create(ARoot: TObject);
    destructor Destroy; override;
    procedure Compile(Lines: TStringList); override; overload;
    property Root: TObject read FRoot;
  end;

procedure ReadProperty(Instance: TObject; PropInfo: PPropInfo; const Value: string);
procedure ReadPropertyValue(Instance: TObject; const PropName: string; const Value: string);

implementation

uses
  StrUtils;

{ TDOMJSONParser }

procedure TDOMJSONParser.SetObject(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin
end;

{ TRTTIJSONParser }

procedure TRTTIJSONParser.SetObject(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin
  ReadPropertyValue(AJSONObject, AJSONName, AJSONValue);
end;

{ TJSONControllerValue }

procedure TJSONControllerValue.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollectorValue) do
  begin
    case Control.Code of
      ctlOpenBlock:
      begin
        Post;
        Parser.Push(TJSONCollectorName.Create(Parser, JSONObject));
      end;
      ctlOpenArray:
      begin
        Post;
        //Parser.Push(TJSONCollectorName.Create(Parser));
      end;
      ctlNext:
      begin
        Post;
        Parser.SetAction([paPop]);
      end;
      ctlStop:
      begin
        Post;
        Parser.SetAction([paPop]);
      end;
      else
        inherited;
    end;
  end;
end;

{ TJSONCollectorValue }

function TJSONCollectorValue.CreateController: TController;
begin
  Result := TJSONControllerValue.Create(Self);
end;

constructor TJSONCollectorValue.Create(AParser: TParser; AName: string; AJSONObject: TObject);
begin
  inherited Create(AParser);
  JSONName := AName;
  JSONObject := AJSONObject;
end;

procedure TJSONCollectorValue.Prepare;
begin
  inherited Prepare;
end;

procedure TJSONCollectorValue.AddToken(Token: TSardToken);
begin
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) or (Token.TokenType = typeNumber) then
  begin
    (Parser as TJSONParser).SetObject(JSONObject, JSONName, DequoteStr(Token.Value));
  end;
end;

{ TJSONControllerName }

procedure TJSONControllerName.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollectorName) do
  begin
    case Control.Code of
      ctlAssign:
      begin
        Post;
        Parser.Push(TJSONCollectorValue.Create(Parser, JSONName, JSONObject));
      end;
      else
        inherited;
    end;
  end;
end;

{ TJSONCollectorName }

function TJSONCollectorName.CreateController: TController;
begin
  Result := TJSONControllerName.Create(Self);
end;

constructor TJSONCollectorName.Create(AParser: TParser; AJSONObject: TObject);
begin
  inherited Create(AParser);
  JSONObject := AJSONObject;
end;

procedure TJSONCollectorName.Prepare;
begin
  inherited Prepare;
end;

procedure TJSONCollectorName.Post;
begin
  inherited;
  if JSONName = '' then
    RaiseError('Name is empty');
end;

procedure TJSONCollectorName.AddToken(Token: TSardToken);
begin
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) then
  begin
    JSONName := DequoteStr(Token.Value, '"');
  end;
end;

{ TJSONCollector }

function TJSONCollector.IsInitial: Boolean;
begin
  Result := false;
end;

procedure TJSONCollector.Reset;
begin

end;

procedure TJSONCollector.Prepare;
begin

end;

procedure TJSONCollector.Post;
begin

end;

procedure TJSONCollector.Next;
begin

end;

{ TControllerNormal }

procedure TControllerNormal.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollector) do
  begin
    case Control.Code of
      ctlStart:
      begin
      end;
      ctlStop:
        Post;
      ctlEnd:
      begin
        Post;
        Next;
      end;
      else
        RaiseError('Not implemented yet :(');
    end;
  end;
end;

{ TJSONScript }

constructor TJSONScript.Create(ARoot: TObject);
begin
  inherited Create;
  FRoot := ARoot;
end;

destructor TJSONScript.Destroy;
begin
  FreeAndNil(Scanner);
  inherited;
end;

procedure TJSONScript.Compile(Lines: TStringList);
begin
  Scanner := TJSONScanner.Create(FRoot);
  Scanner.Scan(Lines);
end;

{ TJSONScanner }

function TJSONScanner.CreateParser: TParser;
begin
  Result := TRTTIJSONParser.Create(Lexer, FRoot);
end;

constructor TJSONScanner.Create(ARoot: TObject);
begin
  inherited Create;
  FRoot := ARoot;
  Add(TJSONLexer.Create);
end;

{ TJSONParser }

procedure TJSONParser.SetToken(Token: TSardToken);
begin
  Current.AddToken(Token);
  DoQueue();
  FActions := [];
  LastControl := ctlToken;
end;

procedure TJSONParser.SetControl(AControl: TSardControl);
begin
  inherited;
  if (LastControl = ctlCloseBlock) then //see setToken
  begin
      LastControl := ctlNone;//prevent loop
      SetControl(Lexer.Controls.GetControl(ctlEnd));
  end;

  Current.AddControl(AControl);
  DoQueue();
  if (paBypass in Actions) then //TODO check if Set work good here
      Current.AddControl(AControl);
  FActions := [];
  LastControl := aControl.Code;
end;

procedure TJSONParser.DoQueue;
begin
  if (paPop in actions) then
  begin
      FActions := FActions - [paPop];
      Pop();
  end;

  if (NextCollector <> nil) then
  begin
      Push(NextCollector);
      FNextCollector := nil;
  end
end;

procedure TJSONParser.SetObject(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin

end;

constructor TJSONParser.Create(ALexer: TLexer; AJSONObject: TObject);
begin
  inherited Create;
  Lexer := ALexer;
  Push(TJSONCollectorName.Create(Self, AJSONObject));
end;

destructor TJSONParser.Destroy;
begin
  Pop;
  inherited;
end;

procedure TJSONParser.Start;
begin
  inherited;
  SetControl(Lexer.Controls.GetControl(ctlStart));
end;

procedure TJSONParser.Stop;
begin
  inherited;
  SetControl(Lexer.Controls.GetControl(ctlStop));
end;

{ TJSONLexer }

constructor TJSONLexer.Create;
begin
  inherited;
  with Symbols do
  begin
  end;

  with Controls do
  begin
    Add('', ctlNone);////TODO i feel it is so bad
    Add('', ctlToken);
    Add('', ctlOperator);
    Add('', ctlStart);
    Add('', ctlStop);

    Add('(', ctlOpenParams);
    Add('[', ctlOpenArray);
    Add('{', ctlOpenBlock);
    Add(')', ctlCloseParams);
    Add(']', ctlCloseArray);
    Add('}', ctlCloseBlock);
    Add(';', ctlEnd);
    Add(',', ctlNext);
    Add(':', ctlAssign);
  end;

  with (Self) do
  begin
      Add(TWhitespace_Tokenizer.Create);
      Add(TComment_Tokenizer.Create);
      Add(TLineComment_Tokenizer.Create);
      Add(TNumber_Tokenizer.Create);
      Add(TDQString_Tokenizer.Create);
      Add(TControl_Tokenizer.Create);
      Add(TIdentifier_Tokenizer.Create);//Sould be last one
  end;
end;

function TJSONLexer.IsEOL(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sEOL);
end;

function TJSONLexer.IsWhiteSpace(vChar: char; vOpen: Boolean): Boolean;
begin
  Result := CharInSet(vChar, sWhitespace);
end;

function TJSONLexer.IsControl(vChar: Char): Boolean;
begin
  Result := Controls.IsOpenBy(vChar);
end;

function TJSONLexer.IsOperator(vChar: Char): Boolean;
begin
  Result := Operators.IsOpenBy(vChar);
end;

function TJSONLexer.IsNumber(vChar: Char; vOpen: Boolean): Boolean;
begin
  if (vOpen) then
    Result := CharInSet(vChar, sNumberOpenChars)
  else
    Result := CharInSet(vChar, sNumberChars);
end;

function TJSONLexer.IsSymbol(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sSymbolChars) or Symbols.IsOpenBy(vChar);
end;

function TJSONLexer.IsIdentifier(vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := inherited isIdentifier(vChar, vOpen); //we do not need to override it, but it is nice to see it here
end;






function StrDateToDate(Value:string): TDateTime;
var
  p:Integer;
  function Fetch(const s:string):Integer;
  var
    i:Integer;
  begin
    Result := 0;
    i := p;
    while True do
    begin
      if (p > Length(s)) or (s[p] in ['-', ':', ' ']) then
      begin
        Result := StrToIntDef(Copy(s, i, p - i), 0);
        Inc(p);
        break;
      end;
      Inc(p);
    end;
  end;
var
  y,m,d,h,n,s: Word;
begin
  p := 1;
  y := Fetch(Value);
  m := Fetch(Value);
  d := Fetch(Value);
  h := Fetch(Value);
  n := Fetch(Value);
  s := Fetch(Value);
  Result := EncodeDate(y, m, d) + EncodeTime(h, n, s, 0);
end;

function GetPropTypeInfo(PropInfo: PPropInfo): PTypeInfo;
begin
{$IFDEF FPC}
  Result := PropInfo^.PropType
{$ELSE}
  Result := PropInfo^.PropType^
{$ENDIF}
end;

function PropType(PropInfo: PPropInfo): TTypeKind;
begin
  Result := PropInfo^.PropType^.Kind;
end;

function IsDefaultValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Int64; //more compatible with FPC
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result := (Default <> LongInt($80000000)) and (Value = Default);
  end;

  function IsDefaultBoolProp: Boolean;
  var
    Value: Int64;
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result := (Default <> LongInt($80000000)) and (Value = Default);
  end;

  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    Result := Value = 0; ;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    Result := Value = 0;
  end;

  function IsDefaultWideStrProp: Boolean;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    Result := VarIsClear(Value);
  end;

  function IsDefaultClassProp: Boolean;
  var
    Value: TObject;
  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    Result := Value = nil;
  end;

  function IsDefaultInterfaceProp: Boolean;
  var
    Value: IInterface;
  begin
    Value := GetInterfaceProp(Instance, PropInfo);
    Result := Value = nil;
  end;
begin
  Result := True; // not default for default :P
  if (PropInfo^.GetProc <> nil) and ((PropInfo^.SetProc <> nil) or (PropInfo^.PropType^.Kind = tkClass) or (PropInfo^.PropType^.Kind = tkInterface)) then
  begin
    PropType := GetPropTypeInfo(PropInfo);
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkWString, tkUString:
        Result := IsDefaultWideStrProp;
      tkString, tkLString:
        Result := IsDefaultStrProp;
      tkMethod: Result := False;
      tkVariant:
        Result := IsDefaultVariantProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
      tkClass:
        Result := IsDefaultClassProp;//TODO:BUG when published Items of collection to inherited parent items
      tkInterface:
        Result := IsDefaultInterfaceProp;
{$IFDEF FPC}
      tkAString:
        Result := IsDefaultStrProp;
      tkBool:
        Result := IsDefaultBoolProp;
{$ENDIF}
    end;
  end;
end;

function ReadVariant(Value:string; ValueType:string): Variant;
var
  aSingle:Single;
  aDouble:Double;
begin
  //need to improve this function
  if (ValueType = 'String') or (ValueType = 'UString') then
    Result := Value
  else if ValueType = 'OleStr' then
    Result := Value
  else if ValueType = 'Byte' then
    Result := Byte(StrToInt(Value))
  else if ValueType = 'ShortInt' then
    Result := SmallInt(StrToInt(Value))
  else if ValueType = 'Word' then
    Result := Word(StrToInt(Value))
  else if ValueType = 'SmallInt' then
    Result := SmallInt(StrToInt(Value))
  else if ValueType = 'Integer' then
    Result := StrToInt(Value)
  else if ValueType = 'Single' then
  begin
    aSingle := StrToFloat(Value);
    Result := aSingle;
  end
  else if ValueType = 'Double' then
  begin
    aDouble := StrToFloat(Value);
    Result := aDouble;
  end
  else if ValueType = 'Currency' then
    Result := StrToCurr(Value)
  else if ValueType = 'Int64' then
    Result := StrToInt64(Value)
  else if ValueType = 'LongWord' then
    Result := StrToInt64(Value)
  else if ValueType = 'Date' then
    Result := StrDateToDate(Value)
  else if ValueType = 'Boolean' then
    Result := StrToBool(Value)
  else
    Result := Value;
end;

procedure ReadProperty(Instance: TObject; PropInfo: PPropInfo; const Value: string);
var
  PropType: PTypeInfo;
  TypeData: PTypeData;

  procedure ReadIntegerProp;
  var
    Data: Longint;
    IdentToInt: TIdentToInt;
  begin
    Data := 0;
    IdentToInt := FindIdentToInt(PropType);
    if not ((Assigned(IdentToInt) and IdentToInt(Value, Data))) then
    begin
      Data := StrToIntDef(Value, 0);
    end;
    SetOrdProp(Instance, PropInfo, Data);
  end;

  procedure ReadBoolProp;
  begin
    SetOrdProp(Instance, PropInfo, Ord(StrToBoolDef(Value, True)));
  end;

  procedure ReadCharProp;
  var
    Data: Longint;
  begin
    if Value <> '' then
    begin
      if (Length(Value)>1) and (Value[1]='#') then
        Data := StrToIntDef(Copy(Value, 2, MaxInt), 0)
      else
        Data := Ord(Value[1]);
    end
    else
      Data := 0;
    SetOrdProp(Instance, PropInfo, Data);
  end;

  procedure ReadEnumeration;
  begin
    SetEnumProp(Instance, PropInfo, Value);
  end;

  procedure ReadSet;
  begin
    SetSetProp(Instance, PropInfo, Value);
  end;

  procedure ReadInt64Prop;
  var
    Data: Int64;
  begin
    Data := StrToInt64(Value);
    SetInt64Prop(Instance, PropInfo, Data);
  end;

  procedure ReadFloatProp;
  var
    Data: Extended;
  begin
    Data := StrToFloat(Value);
    SetFloatProp(Instance, PropInfo, Data);
  end;

  procedure ReadCurrProp;
  var
    Data: Currency;
  begin
    Data := StrToCurr(Value);
    SetFloatProp(Instance, PropInfo, Data);
  end;

  procedure ReadWideStringProp;
  begin
    SetWideStrProp(Instance, PropInfo, widestring(Value));
  end;

  procedure ReadStringProp;
  begin
    SetStrProp(Instance, PropInfo, Value);
  end;

  procedure ReadVariantProp;
  begin
    //SetVariantProp(Instance, PropInfo, ReadVariant(Value, Attributes.Values['ValueType']));
  end;

  procedure ReadObjectProp;
  var
    aObject: TObject;
  begin
    aObject := TObject(GetOrdProp(Instance, PropInfo));
    if (aObject <> nil) and (aObject is TComponent) and not (csSubComponent in (aObject as TComponent).ComponentStyle) then
    begin
      //not now there is long story
    end
    else
    begin
      //(Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, aObject, False));
    end;
  end;

  procedure ReadInterfaceProp;
  var
    aObject: TObject;
  begin
    aObject := TObject(GetOrdProp(Instance, PropInfo));
    //(Owner as TmnXMLRttiReader).Stack.Push((Owner as TmnXMLRttiReader).CreateFiler(PropInfo^.Name, aObject, True));
  end;
begin
  PropType := GetPropTypeInfo(PropInfo);
  TypeData := GetTypeData(PropType);
  case PropType^.Kind of
    tkInteger:
      ReadIntegerProp;
    tkChar:
      ReadCharProp;
    tkSet:
      ReadSet;
    tkEnumeration:
      ReadEnumeration;
    tkInt64:
      ReadInt64Prop;
    tkFloat:
    begin
      if (TypeData <> nil) and (TypeData^.FloatType = ftCurr) then
        ReadCurrProp
      else
        ReadFloatProp;
    end;
    tkWString, tkUString:
      ReadWideStringProp;
    tkLString, tkString:
      ReadStringProp;
    tkVariant:
      ReadVariantProp;
    tkClass:
      ReadObjectProp;
    tkMethod: ; //not yet
    tkInterface:
      ReadInterfaceProp; //not yet
    {$IFDEF FPC}
    tkAString:
      ReadStringProp;
    tkBool:
      ReadBoolProp;
    {$ENDIF}
  end;
end;

procedure ReadPropertyValue(Instance: TObject; const PropName: string; const Value: string);
var
  PropInfo: PPropInfo;
begin
  //need to test the speed
  PropInfo := GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo^.GetProc <> nil) and //i removed IsStoredProp for reader must not check if stored, it is already have default value and not stored
    ((PropInfo^.SetProc <> nil) or
      ((GetOrdProp(Instance, PropInfo) <> 0) and //Must be not null when read properties or must have a SetProc
      (PropInfo^.PropType^.Kind in [tkClass, tkInterface]))) then
  begin
    {$ifdef SAFELOAD}
    try
    {$endif}
    ReadProperty(Instance, PropInfo, Value)
    {$ifdef SAFELOAD}
    except
    end;
    {$endif}
  end
{  else
    SkipProperty(PropName);}
end;

end.
