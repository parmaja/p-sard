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
    JSONName: string;
  protected
    JSONObject: TObject;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; AJSONObject: TObject);
    procedure Prepare; override;
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
    JSONName: string;
    IsPosted: Boolean;
  protected
    JSONObject: TObject;
    function CreateController: TController; override;
  public
    constructor Create(AParser: TParser; AName: string; AJSONObject: TObject);
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

  { TJSONParser }

  TJSONParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    procedure DoQueue;
    function GetSubObject(ParentObject: TObject; out AJSONObject: TObject; AJSONName: string; AJSONObjectClass: string): Boolean; virtual;
    procedure SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string); virtual;
  public
    constructor Create(ALexer: TLexer; AJSONObject: TObject); virtual;
    destructor Destroy; override;

    procedure SetToken(Token: TSardToken); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TJSONParserClass = class of TJSONParser;

  { TRTTIJSONParser }

  TRTTIJSONParser = class(TJSONParser)
  protected
    procedure SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string); override;
  end;

  { TJSONObject }

  TJSONObject = class abstract(TSardObject)
  public
    procedure WriteTo(Strings :TStrings; LastOne:Boolean; Level: Integer); virtual;
  end;

  TJSONValue = class;

  { TJSONElement }

  TJSONElement = class(TJSONObject)
  private
    FJSONClass: string;
    FName: string;
    FValue: TJSONValue;
    procedure SetName(AValue: string);
    procedure SetValue(AValue: TJSONValue);
  public
    constructor Create; virtual;
    procedure WriteTo(Strings :TStrings; LastOne:Boolean; Level: Integer); override;
  published
    property JSONClass: string read FJSONClass write FJSONClass;
    property Value: TJSONValue read FValue write SetValue;
    property Name: string read FName write SetName;
  end;

  TJSONElementClass = class of TJSONElement;

  { TJSONValue }

  TJSONValue = class abstract(TJSONObject)
  private
    FParent: TJSONElement;
  public
    constructor Create(AParent: TJSONElement);
    property Parent: TJSONElement read FParent;
  end;

  TJSONValueClass = class of TJSONValue;

  { TJSONText_Value }

  TJSONText_Value = class(TJSONValue)
  private
    FText: string;
  public
    procedure WriteTo(Strings :TStrings; LastOne:Boolean; Level: Integer); override;
    constructor Create(AParent: TJSONElement; AText: string); overload;
  published
    property Text: string read FText write FText;
  end;

  TJSONList = class(TmnObjectList<TJSONElement>)
  public
  end;

  { TJSONObject }

  TJSONList_Value = class(TJSONValue)
  private
    FItems: TJSONList;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure Add(Value: TJSONElement); overload;
    procedure WriteTo(Strings: TStrings; LastOne:Boolean; Level: Integer); override;
    property Items: TJSONList read FItems;
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
    function GetSubObject(ParentObject: TObject; out AJSONObject: TObject; AJSONName: string; AJSONObjectClass: string): Boolean; override;
    procedure SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string); override;
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

{ TJSONValue }

constructor TJSONValue.Create(AParent: TJSONElement);
begin
  inherited Create;
  FParent := AParent;
end;

{ TJSONObject }

procedure TJSONObject.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
begin

end;

{ TJSONText_Value }

procedure TJSONText_Value.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
var
  Line: string;
begin
  inherited;
  Line := StringOfChar(' ', Level * 4) + QuoteStr(Parent.Name, '"')+': '+QuoteStr(Text, '"');
  if not LastOne then
    Line := Line + ',';
  Strings.Add(Line);
end;

constructor TJSONText_Value.Create(AParent: TJSONElement; AText: string);
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

{ TJSONList_Value }

procedure TJSONList_Value.Created;
begin
  inherited;
  FItems := TJSONList.Create;
end;

destructor TJSONList_Value.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJSONList_Value.Add(Value: TJSONElement);
begin
  Items.Add(Value);
end;

procedure TJSONList_Value.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
var
  Itm: TJSONElement;
begin
  inherited;
  Strings.Add(StringOfChar(' ', Level * 4) + QuoteStr(Parent.Name, '"') + ': {');
  for Itm in Items do
    Itm.WriteTo(Strings, itm = Items.Last , Level + 1);
  if not LastOne then
    Strings.Add(StringOfChar(' ', Level * 4) + '},')
  else
    Strings.Add(StringOfChar(' ', Level * 4) + '}');
end;

{ TJSONElement }

procedure TJSONElement.SetValue(AValue: TJSONValue);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
end;

procedure TJSONElement.SetName(AValue: string);
begin
  if FName =AValue then Exit;
  FName :=AValue;
end;

constructor TJSONElement.Create;
begin
  inherited Create;
end;

procedure TJSONElement.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
begin
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

function TDOMJSONParser.GetSubObject(ParentObject: TObject; out AJSONObject: TObject; AJSONName: string; AJSONObjectClass: string): Boolean;
var
  Item: TJSONClass;
begin
  Item := JSONClasses.Find(AJSONObjectClass);
  if Item = nil then
    RaiseError('We do not have: ' + AJSONObjectClass);

  AJSONObject := Item.JSONClass.Create(ParentObject as TJSONElement);
  (AJSONObject as TJSONElement).Name := AJSONName;
  (AJSONObject as TJSONElement).JSONClass := AJSONObjectClass;
  (ParentObject as TJSONList_Value).Add(AJSONObject as TJSONElement);
  Result := True;
end;

procedure TDOMJSONParser.SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin
  //(AJSONObject as TJSONList_Value).Add(TJSONText_Value.Create(AJSONValue));
//  (AJSONObject as TJSONText_Value).Value := AJSONValue;
end;

procedure TDOMJSONParser.Created;
begin
  inherited Created;
  JSONClasses := TJSONClasses.Create;
  JSONClasses.Add('', TJSONText_Value);
  JSONClasses.Add('Value', TJSONText_Value);
  JSONClasses.Add('Object', TJSONList_Value);
  JSONClasses.Add('Array', TJSONArray_Value);
end;

destructor TDOMJSONParser.Destroy;
begin
  FreeAndNil(JSONClasses);
  inherited Destroy;
end;

{ TRTTIJSONParser }

procedure TRTTIJSONParser.SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin
  SetPropertyValue(AJSONObject, AJSONName, AJSONValue);
end;

{ TJSONControllerValue }

procedure TJSONControllerValue.SetControl(Control: TSardControl);
var
  AJSONObject: TObject;
begin
  with (Collector as TJSONCollectorValue) do
  begin
    case Control.Code of
      ctlOpenBlock:
      begin
        Post;
        (Parser as TJSONParser).GetSubObject(JSONObject, AJSONObject, JSONName, 'Object');
        if AJSONObject = nil then
          RaiseError('Can not find object: ' + JSONName);

        Parser.Push(TJSONCollectorElement.Create(Parser, AJSONObject));

      end;
      ctlCloseBlock:
      begin
        Post;
        Parser.SetAction([paPop]);
      end;
      ctlOpenArray:
      begin
        Post;
        //Parser.Push(TJSONCollectorElement.Create(Parser));
      end;
      ctlNext:
      begin
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
  IsPosted := False;
  JSONName := AName;
  JSONObject := AJSONObject;
end;

procedure TJSONCollectorValue.Reset;
begin
  inherited Reset;
  JSONName := '';
  IsPosted:= False;
end;

procedure TJSONCollectorValue.Post;
begin
  inherited Post;
  IsPosted := True;
end;

procedure TJSONCollectorValue.SetToken(Token: TSardToken);
begin
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) or (Token.TokenType = typeNumber) then
  begin
    if IsPosted then
      RaiseError('Value is already set');
    (Parser as TJSONParser).SetObjectValue(JSONObject, JSONName, DequoteStr(Token.Value));
    Post;
    Parser.SetAction([paPop]);
  end;
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
        Parser.Push(TJSONCollectorValue.Create(Parser, JSONName, JSONObject));
      end;
      ctlNext:
      begin
        Reset;
      end;
      ctlCloseBlock:
      begin
        Post;
        Parser.SetAction([paPop]);
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

constructor TJSONCollectorElement.Create(AParser: TParser; AJSONObject: TObject);
begin
  inherited Create(AParser);
  JSONObject := AJSONObject;
end;

procedure TJSONCollectorElement.Prepare;
begin
  inherited Prepare;
end;

procedure TJSONCollectorElement.Reset;
begin
  inherited Reset;
  JSONName := '';
end;

procedure TJSONCollectorElement.Post;
begin
  inherited;
  if JSONName = '' then
    RaiseError('Name is empty');
end;

procedure TJSONCollectorElement.SetToken(Token: TSardToken);
begin
  if (Token.TokenType = typeIdentifier) or (Token.TokenType = typeString) then
  begin
    if JSONName <> '' then
      RaiseError('Name already set: ' + JSONName);
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

function TJSONParser.GetSubObject(ParentObject: TObject; out AJSONObject: TObject; AJSONName: string; AJSONObjectClass: string): Boolean;
begin
  Result := False;
  AJSONObject := nil;
end;

procedure TJSONParser.SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin

end;

constructor TJSONParser.Create(ALexer: TLexer; AJSONObject: TObject);
begin
  inherited Create;
  Lexer := ALexer;
  Push(TJSONCollectorElement.Create(Self, AJSONObject));
  Push(TJSONCollectorValue.Create(Self, '', AJSONObject));
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
