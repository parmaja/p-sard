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
  mnUtils, mnClasses, mnRTTIUtils,
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
  public
    function IsInitial: Boolean; override;
    procedure Reset; override;
    procedure Prepare; override;
    procedure Post; override;
    procedure Next; override;
  end;

  { TJSONControllerElement }

  TJSONControllerElement = class(TJSONController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollectorElement }

  TJSONCollectorElement = class(TJSONCollector)
  private
    Name: string;
  protected
    ParentObject: TObject;
    CurrentObject: TObject;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; AParentObject, ACurrentObject: TObject);
    procedure Reset; override;
    procedure Post; override;
    procedure SetToken(Token: TSardToken); override;
  end;

  { TJSONControllerValue }

  TJSONControllerValue = class(TJSONController)
  protected
  public
    procedure SetControl(Control: TSardControl); override;
  end;

  { TJSONCollectorValue }

  TJSONCollectorValue = class(TJSONCollector)
  private
    IsPosted: Boolean;
  protected
    CurrentObject: TObject;
    Name: string;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; AName: string; AObject: TObject);
    procedure Reset; override;
    procedure Post; override;
    procedure SetToken(Token: TSardToken); override;
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

  TJSONType = (
    jtString,
    jtNumber,
    jtObject,
    jtArray
  );

  { TJSONParser }

  TJSONParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    procedure DoQueue;
    procedure NeedObject(AParentObject: TObject; AName: string; out AObject: TObject); virtual;
    procedure SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType); virtual;
  public
    constructor Create(ALexer: TLexer; AObject: TObject); virtual;
    destructor Destroy; override;

    procedure SetToken(Token: TSardToken); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TJSONParserClass = class of TJSONParser;

  TSourceWriter = class abstract(TObject)
  public
    procedure Add(S: string); virtual; abstract;
    procedure NewLine; virtual; abstract;
  end;

  { TStringSourceWriter }

  TStringSourceWriter = class(TSourceWriter)
  private
    FStrings: TStrings;
    FLine: string;
  public
    constructor Create(Strings: TStrings);
    destructor Destroy; override;
    procedure Add(S: string); override;
    procedure NewLine; override;
  end;

  { TRTTIJSONParser }

  TRTTIJSONParser = class(TJSONParser)
  protected
    procedure SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType); override;
  end;

  { TJSONObject }

  TJSONObject = class abstract(TSardObject)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); virtual;
  end;

  TJSONValue = class;

  { TJSONElement }

  TJSONElement = class(TJSONObject)
  private
    FName: string;
    FValue: TJSONValue;
    procedure SetName(AValue: string);
    procedure SetValue(AValue: TJSONValue);
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  published
    property Value: TJSONValue read FValue write SetValue;
    property Name: string read FName write SetName;
  end;

  { TJSONRoot }

  TJSONRoot = class(TJSONElement)
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
  end;

  TJSONElementClass = class of TJSONElement;

  { TJSONValue }

  TJSONValue = class abstract(TJSONObject)
  private
    FParent: TJSONElement;
  public
    constructor Create(AParent: TJSONElement);
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Parent: TJSONElement read FParent;
  end;

  TJSONValueClass = class of TJSONValue;

  { TJSONString_Value }

  TJSONString_Value = class(TJSONValue)
  private
    FText: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONElement; AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  { TJSONNumber_Value }

  TJSONNumber_Value = class(TJSONValue)
  private
    FNumber: string;
  public
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONElement; ANumber: string); overload;
  published
    property Number: string read FNumber write FNumber;
  end;

  TJSONItems = class(TmnObjectList<TJSONElement>)
  public
  end;

  TJSONObject_Value = class(TJSONValue)
  private
    FItems: TJSONItems;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure Add(Value: TJSONElement); overload;
    procedure WriteTo(Writer: TSourceWriter; LastOne:Boolean; Level: Integer); override;
    property Items: TJSONItems read FItems;
  published
  end;

  { TJSONArray }

  TJSONArray_Value = class(TJSONValue)
  private
    FItems: TStrings;
  public
    procedure Created; override;
    destructor Destroy; override;
    property Items: TStrings read FItems;
  published
  end;

//--------------------

 { TDOMJSONParser }

  TDOMJSONParser = class(TJSONParser)
  protected
    type
      TJSONClass = class(TmnNamedObject)
      public
        JSONClass: TJSONValueClass;
      end;

      { TJSONClasses }

      TJSONClasses = class(TmnNamedObjectList<TJSONClass>)
      public
        procedure Add(AName: string; AValueClass: TJSONValueClass);
      end;

  protected
    JSONClasses: TJSONClasses;
    procedure NeedObject(AParentObject: TObject; AJSONName: string; out AJSONObject: TObject); override;
    procedure SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType); override;
  public
    procedure Created; override;
    destructor Destroy; override;
  end;

  { TJSONScanner }

  TJSONScanner = class(TScanner)
  protected
    FParserClass: TJSONParserClass;
    FRoot: TObject;
    function CreateParser: TParser; override;
  public
    constructor Create(ARoot: TObject; AParserClass: TJSONParserClass);
    procedure Compile(Lines: TStringList);
    property Root: TObject read FRoot;
  end;

implementation

uses
  StrUtils;

{ TStringSourceWriter }

constructor TStringSourceWriter.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

destructor TStringSourceWriter.Destroy;
begin
  if FLine <> '' then
    NewLine;
  inherited Destroy;
end;

procedure TStringSourceWriter.Add(S: string);
begin
  FLine := FLine + S;
end;

procedure TStringSourceWriter.NewLine;
begin
  FStrings.Add(FLine);
  FLine := '';
end;

{ TJSONRoot }

procedure TJSONRoot.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Value.WriteTo(Writer, LastOne, Level);
end;

{ TJSONNumber_Value }

procedure TJSONNumber_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  inherited;
  Writer.Add(Number);
end;

constructor TJSONNumber_Value.Create(AParent: TJSONElement; ANumber: string);
begin
  inherited Create(AParent);
  FNumber := ANumber;
end;

{ TJSONValue }

constructor TJSONValue.Create(AParent: TJSONElement);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TJSONValue.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  if Self = nil then
    Writer.Add('null');
end;

{ TJSONObject }

procedure TJSONObject.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin

end;

{ TJSONString_Value }

procedure TJSONString_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(QuoteStr(Text, '"'));
end;

constructor TJSONString_Value.Create(AParent: TJSONElement; AText: string);
begin
  inherited Create(AParent);
  FText := AText;
end;

{ TJSONArray_Value }

procedure TJSONArray_Value.Created;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TJSONArray_Value.Destroy;
begin
  FreeAndNil(FItems);
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

procedure TJSONObject_Value.Add(Value: TJSONElement);
begin
  Items.Add(Value);
end;

procedure TJSONObject_Value.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
var
  Itm: TJSONElement;
begin
  inherited;
  Writer.Add('{');
  Writer.NewLine;
  for Itm in Items do
    Itm.WriteTo(Writer, itm = Items.Last , Level + 1);
  Writer.Add(StringOfChar(' ', Level * 4) + '}');
  Writer.NewLine;
end;

{ TJSONElement }

procedure TJSONElement.SetValue(AValue: TJSONValue);
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

procedure TJSONElement.SetName(AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

procedure TJSONElement.WriteTo(Writer: TSourceWriter; LastOne: Boolean; Level: Integer);
begin
  Writer.Add(StringOfChar(' ', Level * 4) + QuoteStr(Name, '"') + ': ');
  if Value = nil then
    RaiseError('Value is null for: ' + Name)
  else
    Value.WriteTo(Writer, LastOne, Level);
  if not LastOne then
    Writer.Add(',');
  Writer.NewLine;
end;

{ TJSONClasses }

procedure TDOMJSONParser.TJSONClasses.Add(AName: string; AValueClass: TJSONValueClass);
var
  Item: TJSONClass;
begin
  Item := TJSONClass.Create;
  Item.Name := AName;
  Item.JSONClass := AValueClass;
  inherited Add(Item);
end;

{ TDOMJSONParser }

procedure TDOMJSONParser.NeedObject(AParentObject: TObject; AJSONName: string; out AJSONObject: TObject);
begin
  if not ((AParentObject as TJSONElement).Value is TJSONObject_Value) then
    RaiseError('Value is not object');
  AJSONObject := TJSONElement.Create;
  (AJSONObject as TJSONElement).Name := AJSONName;
  ((AParentObject as TJSONElement).Value as TJSONObject_Value).Add((AJSONObject as TJSONElement));
end;

procedure TDOMJSONParser.SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType);
begin
  if (AObject as TJSONElement).Value <> nil then
    RaiseError('Value is already set');
  case AType of
    jtNumber: (AObject as TJSONElement).Value :=  TJSONNumber_Value.Create(nil, AValue);
    jtString: (AObject as TJSONElement).Value :=  TJSONString_Value.Create(nil, AValue);
    jtObject: (AObject as TJSONElement).Value := TJSONObject_Value.Create(nil);
  end;
end;

procedure TDOMJSONParser.Created;
begin
  inherited Created;
  JSONClasses := TJSONClasses.Create;
  JSONClasses.Add('', TJSONString_Value);
  JSONClasses.Add('String', TJSONString_Value);
  JSONClasses.Add('Number', TJSONNumber_Value);
  JSONClasses.Add('Object', TJSONObject_Value);
  JSONClasses.Add('Array', TJSONArray_Value);
end;

destructor TDOMJSONParser.Destroy;
begin
  FreeAndNil(JSONClasses);
  inherited Destroy;
end;

{ TRTTIJSONParser }

procedure TRTTIJSONParser.SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType);
begin
  SetPropertyValue(AObject, AName, AValue);
end;

{ TJSONControllerValue }

procedure TJSONControllerValue.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollectorValue) do
  begin
    case Control.Code of
      ctlOpenBlock:
      begin
        (Parser as TJSONParser).SetObjectValue(CurrentObject, Name, '', jtObject);
        Parser.Push(TJSONCollectorElement.Create(Parser, CurrentObject, nil));
      end;
      ctlCloseBlock:
      begin
        Parser.SetAction([paPop]);
      end;
      ctlOpenArray:
      begin
        (Parser as TJSONParser).SetObjectValue(CurrentObject, Name, '', jtArray);
        Parser.Push(TJSONCollectorElement.Create(Parser, CurrentObject, nil));
      end;
      ctlNext:
      begin
        Parser.SetAction([paPop]);
      end;
      ctlStop:
      begin
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

constructor TJSONCollectorValue.Create(AParser: TParser; AName: string; AObject: TObject);
begin
  inherited Create(AParser);
  IsPosted := False;
  Name := AName;
  CurrentObject := AObject;
end;

procedure TJSONCollectorValue.Reset;
begin
  inherited Reset;
end;

procedure TJSONCollectorValue.Post;
begin
  inherited Post;
  Reset;
end;

procedure TJSONCollectorValue.SetToken(Token: TSardToken);
begin
  if IsPosted then
    RaiseError('Value is already set');
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) then
    (Parser as TJSONParser).SetObjectValue(CurrentObject, Name, DequoteStr(Token.Value), jtString)
  else if (Token.TokenType = typeNumber) then
    (Parser as TJSONParser).SetObjectValue(CurrentObject, Name, DequoteStr(Token.Value), jtNumber);
  Parser.SetAction([paPop]);
end;

{ TJSONControllerElement }

procedure TJSONControllerElement.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollectorElement) do
  begin
    case Control.Code of
      ctlAssign:
      begin
        Post;
        Parser.Push(TJSONCollectorValue.Create(Parser, Name, CurrentObject));
      end;
      ctlNext:
      begin
        Reset;
        Parser.SetAction([paPop], TJSONCollectorElement.Create(Parser, ParentObject, nil));
        //RaiseError('You need to set value for this object: ' + Name);
      end;
      ctlCloseBlock:
      begin
        Parser.SetAction([paPop], TJSONCollectorElement.Create(Parser, ParentObject, nil));
      end;
      else
        inherited;
    end;
  end;
end;

{ TJSONCollectorElement }

function TJSONCollectorElement.CreateController: TController;
begin
  Result := TJSONControllerElement.Create(Self);
end;

constructor TJSONCollectorElement.Create(AParser: TParser; AParentObject, ACurrentObject: TObject);
begin
  inherited Create(AParser);
  CurrentObject := ACurrentObject;
  ParentObject := AParentObject;
end;

procedure TJSONCollectorElement.Reset;
begin
  inherited Reset;
  Name := '';
end;

procedure TJSONCollectorElement.Post;
begin
  inherited;
  (Parser as TJSONParser).NeedObject(ParentObject, Name, CurrentObject);
end;

procedure TJSONCollectorElement.SetToken(Token: TSardToken);
begin
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) then
  begin
    if Name <> '' then
      RaiseError('Name already set: ' + Name);
    Name := DequoteStr(Token.Value, '"');
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

procedure TJSONController.SetControl(Control: TSardControl);
begin
  with (Collector as TJSONCollector) do
  begin
    case Control.Code of
      ctlStart:
      begin
      end;
      ctlStop: ;
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

{ TJSONScanner }

function TJSONScanner.CreateParser: TParser;
begin
  Result := FParserClass.Create(Current, FRoot);
end;

constructor TJSONScanner.Create(ARoot: TObject; AParserClass: TJSONParserClass);
begin
  inherited Create;
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

  if Current = nil then
    RaiseError('There is no current collector');
  Current.SetControl(AControl);
  DoQueue();
  if (paPass in Actions) then //TODO check if Set work good here
    Current.SetControl(AControl);
  FActions := [];
  LastControl := aControl.Code;
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

procedure TJSONParser.NeedObject(AParentObject: TObject; AName: string; out AObject: TObject);
begin
  AObject := nil;
end;

procedure TJSONParser.SetObjectValue(AObject: TObject; AName: string; AValue: string; AType: TJSONType);
begin
end;

constructor TJSONParser.Create(ALexer: TLexer; AObject: TObject);
begin
  inherited Create;
  Lexer := ALexer;
  Push(TJSONCollectorElement.Create(Self, nil, AObject));
  Push(TJSONCollectorValue.Create(Self, '', AObject));
end;

destructor TJSONParser.Destroy;
begin
  inherited;
end;

procedure TJSONParser.Start;
begin
  inherited;
  if Current <> nil then
    SetControl(Lexer.Controls.GetControl(ctlStart));
end;

procedure TJSONParser.Stop;
begin
  inherited;
  if Current <> nil then //not already finished
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

end.
