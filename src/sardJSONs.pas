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

    empty array

    "statue" = []

    --escape--
    "name": "The \"Escape",
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

  { TJSONCollectorElement }

  {
    Element is Name and Value, Value can be String/Object/Array
  }

  TJSONCollectorElement = class(TJSONCollector)
  private
    Name: string;
  protected
    type
      TExpect = (elmName, elmValue);
    var
      Expect: TExpect;
    ParentElement: TObject;
    CurrentObject: TObject;
  public
    constructor Create(AParser: TParser; AParentObject: TObject; ACurrentObject: TObject = nil);
    procedure Reset; override;
    procedure DoToken(Token: TSardToken); override;
    procedure DoControl(AControl: TSardControl); override;
  end;

  { TJSONCollectorValue }

  TJSONCollectorValue = class(TJSONCollector)
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

  { TJSONCollectorArray }

  TJSONCollectorArray = class(TJSONCollector)
  private
  protected
    type
      TExpect = (aryValue, aryNext);
    var
      Expect: TExpect;
    ParentObject: TObject; //element with array value created
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
    procedure DoQueue;
    procedure RequireElement(AParentObject: TObject; const AName: string; out AObject: TObject); virtual;
    function SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject; virtual; abstract;
  public
    constructor Create(ALexer: TLexer; AObject: TObject); virtual;
    destructor Destroy; override;

    procedure SetToken(Token: TSardToken); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure Start; override;
    procedure Stop; override;

    property Strict: Boolean read FStrict write FStrict default true;
  end;

  TJSONParserClass = class of TJSONParser;

//-----------------------------------------------------------------------------

  { TJSONBase }

  TJSONBase = class abstract(TSardObject)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); virtual;
  end;

  TJSONValue = class;

  { TJSONObject }

  TJSONObject = class(TJSONBase)
  private
    FName: string;
    FValue: TJSONValue;
    procedure SetName(const AValue: string);
    procedure SetValue(AValue: TJSONValue);
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  published
    property Value: TJSONValue read FValue write SetValue;
    property Name: string read FName write SetName;
  end;

  { TJSONRoot }

  TJSONRoot = class(TJSONObject)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  end;

  TJSONElementClass = class of TJSONObject;

  { TJSONValue }

  TJSONValue = class abstract(TJSONBase)
  private
    FParent: TJSONObject;
  public
    constructor Create(AParent: TJSONObject);
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Parent: TJSONObject read FParent;
  end;

  TJSONValueClass = class of TJSONValue;

  { TJSONString_Value }

  TJSONString_Value = class(TJSONValue)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONObject; const AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TJSONIdentifier_Value }

  TJSONIdentifier_Value = class(TJSONValue)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONObject; const AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TJSONNumber_Value }

  TJSONNumber_Value = class(TJSONValue)
  private
    FNumber: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONObject; const ANumber: string); overload;
  published
    property Number: string read FNumber write FNumber;
  end;

  { TJSONBoolean_Value }

  TJSONBoolean_Value = class(TJSONValue)
  private
    FValue: Boolean;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONObject; AValue: Boolean); overload;
    constructor Create(AParent: TJSONObject; const AValue: string); overload;
  published
    property Value: Boolean read FValue write FValue;
  end;

  { TJSONItems }

  TJSONItems = class(TmnObjectList<TJSONObject>)
  public
  end;

  { TJSONObject_Value }

  TJSONObject_Value = class(TJSONValue)
  private
    FItems: TJSONItems;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure RequireElement(const AJSONName: string; out AJSONObject: TObject);
    procedure Add(Value: TJSONObject); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Items: TJSONItems read FItems;
  published
  end;

  { TJSONList }

  TJSONList = class(TmnObjectList<TJSONValue>)
  public
  end;

  { TJSONArray_Value }

  TJSONArray_Value = class(TJSONValue)
  private
    FItems: TJSONList;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure RequireElement(const AJSONName: string; out AJSONObject: TObject);
    procedure Add(Value: TJSONValue); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer); override;
    property Items: TJSONList read FItems;
  published
  end;

//-----------------------------------------------------------------------------

 { TDataJSONParser }

  TDataJSONParser = class(TJSONParser)
  protected
  protected
    procedure RequireElement(AParentObject: TObject; const AJSONName: string; out AJSONObject: TObject); override;
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
    property Root: TObject read FRoot;
    property Strict: Boolean read FStrict write FStrict default true;
  end;

implementation

uses
  StrUtils;

{ TJSONBoolean_Value }

procedure TJSONBoolean_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Value then
    Writer.Add('true')
  else
    Writer.Add('false');
  inherited WriteTo(Writer, LastOne, Level);
end;

constructor TJSONBoolean_Value.Create(AParent: TJSONObject; AValue: Boolean);
begin
  inherited Create(AParent);
  FValue := AValue;
end;

constructor TJSONBoolean_Value.Create(AParent: TJSONObject; const AValue: string);
begin
  inherited Create(AParent);
  if AValue = 'true' then
    FValue := true
  else if AValue = 'false' then
    FValue := false;
end;

{ TJSONIdentifier_Value }

procedure TJSONIdentifier_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Text);
  inherited;
end;

constructor TJSONIdentifier_Value.Create(AParent: TJSONObject; const AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

{ TJSONCollectorArray }

constructor TJSONCollectorArray.Create(AParser: TParser; const AName: string; AParentObject: TObject);
begin
  inherited Create(AParser);
  Name := AName;
  ParentObject := AParentObject;
end;

procedure TJSONCollectorArray.Reset;
begin
  inherited Reset;
  Expect := Low(Expect);
end;

procedure TJSONCollectorArray.DoToken(Token: TSardToken);
begin
  RaiseError('No token for array!');
end;

procedure TJSONCollectorArray.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlOpenArray:
    begin
      Parser.Push(TJSONCollectorValue.Create(Parser, Name, ParentObject));
    end;
    ctlCloseArray:
    begin
      Parser.SetAction([paPop]);
      Reset;
    end;
    ctlNext:
    begin
      Parser.Push(TJSONCollectorValue.Create(Parser, Name, ParentObject));
      Reset;
    end;
    else
      inherited;
  end;
end;

{ TJSONRoot }

procedure TJSONRoot.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Value.WriteTo(Writer, LastOne, Level);
end;

{ TJSONNumber_Value }

procedure TJSONNumber_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Number);
  inherited;
end;

constructor TJSONNumber_Value.Create(AParent: TJSONObject; const ANumber: string);
begin
  inherited Create(AParent);
  FNumber := ANumber;
end;

{ TJSONValue }

constructor TJSONValue.Create(AParent: TJSONObject);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TJSONValue.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Self = nil then
    Writer.Add('null');
  if not LastOne then
    Writer.Add(',');
  Writer.NewLine;
end;

{ TJSONBase }

procedure TJSONBase.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin

end;

{ TJSONString_Value }

procedure TJSONString_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(QuoteStr(EscapeStringC(Text), '"'));
  inherited;
end;

constructor TJSONString_Value.Create(AParent: TJSONObject; const AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

{ TJSONArray_Value }

procedure TJSONArray_Value.Created;
begin
  inherited;
  FItems := TJSONList.Create;
end;

destructor TJSONArray_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJSONArray_Value.RequireElement(const AJSONName: string; out AJSONObject: TObject);
begin

end;

procedure TJSONArray_Value.Add(Value: TJSONValue);
begin
  Items.Add(Value);
end;

procedure TJSONArray_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TJSONValue;
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

{ TJSONObject_Value }

procedure TJSONObject_Value.Created;
begin
  inherited;
  FItems := TJSONItems.Create;
end;

destructor TJSONObject_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJSONObject_Value.RequireElement(const AJSONName: string; out AJSONObject: TObject);
begin
  AJSONObject := TJSONObject.Create;
  (AJSONObject as TJSONObject).Name := AJSONName;
  Add((AJSONObject as TJSONObject));
end;

procedure TJSONObject_Value.Add(Value: TJSONObject);
begin
  Items.Add(Value);
end;

procedure TJSONObject_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TJSONObject;
begin
  Writer.Add('{');
  Writer.NewLine;
  for Itm in Items do
    Itm.WriteTo(Writer, itm = Items.Last , Level + 1);
  Writer.Add(Level, '}');
  inherited;
end;

{ TJSONObject }

procedure TJSONObject.SetValue(AValue: TJSONValue);
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

procedure TJSONObject.SetName(const AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

procedure TJSONObject.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(Level, QuoteStr(Name, '"') + ': ');
  if Value = nil then
    RaiseError('Value is null for: ' + Name)
  else
    Value.WriteTo(Writer, LastOne, Level);
end;

{ TDataJSONParser }

procedure TDataJSONParser.RequireElement(AParentObject: TObject; const AJSONName: string; out AJSONObject: TObject);
begin
  if not (AParentObject is TJSONObject_Value) then
    RaiseError('Value is not object');
  (AParentObject as TJSONObject_Value).RequireElement(AJSONName, AJSONObject);
end;

function TDataJSONParser.SetObjectValue(AObject: TObject; const AName: string; const AValue: string; AType: TJSONType): TObject;
var
  v: TJSONValue;
  procedure CreateValue;
  begin
    case AType of
      jtNumber: v :=  TJSONNumber_Value.Create(nil, AValue);
      jtIdentifier: v :=  TJSONIdentifier_Value.Create(nil, AValue);
      jtBoolean: v :=  TJSONBoolean_Value.Create(nil, AValue);
      jtString: v :=  TJSONString_Value.Create(nil, AValue);
      jtObject: v := TJSONObject_Value.Create(nil);
      jtArray: v := TJSONArray_Value.Create(nil);
    end;
    Result := v;
  end;
begin
  if AObject = nil then
    RaiseError('Can not set value to nil object');

  if (AObject is TJSONArray_Value) then
  begin
    CreateValue;
    (AObject as TJSONArray_Value).Add(v);
  end
  else if (AObject is TJSONObject) then
  begin
     if (AObject as TJSONObject).Value <> nil then
      RaiseError('Value is already set and it is not array: ' + AObject.ClassName);
    CreateValue;
    (AObject as TJSONObject).Value  :=  v;
  end
  else
    RaiseError('Value can not set to:' + AObject.ClassName);
end;

{ TJSONCollectorValue }

constructor TJSONCollectorValue.Create(AParser: TParser; const AName: string; AObject: TObject);
begin
  inherited Create(AParser);
  if AObject = nil then
    RaiseError('Object nil in collection value!');
  Name := AName;
  CurrentObject := AObject;
end;

procedure TJSONCollectorValue.Reset;
begin
  inherited Reset;
  Expect := Low(Expect);
end;

procedure TJSONCollectorValue.DoToken(Token: TSardToken);
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

procedure TJSONCollectorValue.DoControl(AControl: TSardControl);
begin
  case AControl.Code of
    ctlOpenBlock:
    begin
      if Expect = valValue then
      begin
        Parser.Push(TJSONCollectorElement.Create(Parser, CurrentObject, Parser.SetObjectValue(CurrentObject, Name, '', jtObject)));
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
        Parser.Push(TJSONCollectorArray.Create(Parser, Name, Parser.SetObjectValue(CurrentObject, Name, '', jtArray)));
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

{ TJSONCollectorElement }

constructor TJSONCollectorElement.Create(AParser: TParser; AParentObject: TObject; ACurrentObject: TObject);
begin
  inherited Create(AParser);
  ParentElement := AParentObject;
  CurrentObject := ACurrentObject;
end;

procedure TJSONCollectorElement.Reset;
begin
  inherited Reset;
  Name := '';
  Expect := Low(Expect);
end;

procedure TJSONCollectorElement.DoToken(Token: TSardToken);
begin
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) then
  begin
    if Expect = elmName then
    begin
      if Name <> '' then
        RaiseError('Name already set: ' + Name);
      Name := Token.Value;
    end
    else
      RaiseError('Name expected');
  end
  else
    RaiseError('Name expected');
end;

procedure TJSONCollectorElement.DoControl(AControl: TSardControl);
var
  AObject: TObject;
begin
  case AControl.Code of
    ctlStart:
      Parser.Push(TJSONCollectorValue.Create(Parser, '', ParentElement));
    ctlAssign:
    begin
      if Expect = elmName then
      begin
        Parser.RequireElement(CurrentObject, Name, AObject);
        Parser.Push(TJSONCollectorValue.Create(Parser, Name, AObject));
        Inc(Expect);
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

procedure TJSONScanner.Compile(const Text: string);
begin
  Scan(Text);
end;

constructor TJSONScanner.Create(ARoot: TObject; AParserClass: TJSONParserClass);
begin
  inherited Create;
  FStrict := True;
  FRoot := ARoot;
  FParserClass := AParserClass;
  Add(TJSONLexer.Create);
end;

procedure TJSONScanner.Compile(Lines: TStringList);
begin
  Scan(Lines);
end;

{ TJSONParser }

procedure TJSONParser.SetToken(Token: TSardToken);
begin
  Current.SetToken(Token);
  DoQueue;
  FActions := [];
end;

procedure TJSONParser.SetControl(AControl: TSardControl);
begin
  inherited;
  if Current = nil then
    RaiseError('There is no current collector');
  Current.SetControl(AControl);
  while true do
  begin
    DoQueue;
    if (paPass in Actions) then
    begin
      FActions := FActions - [paPass];
      Current.SetControl(AControl)
    end
    else
      break;
  end;
end;

procedure TJSONParser.DoQueue;
begin
  if (paPop in actions) then
  begin
    FActions := FActions - [paPop];
    Pop;
  end;

  if (NextCollector <> nil) then
  begin
    Push(NextCollector);
    FNextCollector := nil;
  end
end;

procedure TJSONParser.RequireElement(AParentObject: TObject; const AName: string; out AObject: TObject);
begin
  AObject := nil;
end;

constructor TJSONParser.Create(ALexer: TLexer; AObject: TObject);
begin
  inherited Create(False); //TODO check if own
  FStrict := True;
  Lexer := ALexer;
  Push(TJSONCollectorElement.Create(Self, AObject));
end;

destructor TJSONParser.Destroy;
begin
  inherited;
end;

procedure TJSONParser.Start;
begin
  inherited;
  if Current <> nil then
    SetControl(ControlStart);
end;

procedure TJSONParser.Stop;
begin
  inherited;
  if Current <> nil then //not already finished
    SetControl(ControlStop);
end;

{ TJSONLexer }

constructor TJSONLexer.Create;
begin
  inherited;

  Add(TWhitespace_Tokenizer.Create);
  Add(TComment_Tokenizer.Create);
  //Add(TLineComment_Tokenizer.Create);
  Add(TNumber_Tokenizer.Create);
  Add(TSL_DQ_String_Tokenizer.Create);
  //Add(TDQString_Tokenizer.Create);

//  Add(TControl_Tokenizer.Create);

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
