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
    function GetObject(AJSONName: string; AJSONObjectClass: string; out AJSONObject: TObject): Boolean; virtual;
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

  { TJSONBaseObject }

  TJSONBase = class(TmnNamedObject)
  private
    FJSONClass: string;
  public
    constructor Create; virtual;
    procedure WriteTo(Strings :TStrings; LastOne:Boolean; Level: Integer); virtual;
  published
    property JSONClass: string read FJSONClass write FJSONClass;
  end;

  TJSONBaseClass = class of TJSONBase;

  { TJSONValue }

  TJSONValue = class(TJSONBase)
  private
    FValue: string;
  public
    procedure WriteTo(Strings :TStrings; LastOne:Boolean; Level: Integer); override;
  published
    property Value: string read FValue write FValue;
  end;

  TJSONObjects = class(TmnNamedObjectList<TJSONBase>)
  public
  end;

  { TJSONObject }

  TJSONObject = class(TJSONBase)
  private
    FItems: TJSONObjects;
  public
    procedure Created; override;
    destructor Destroy; override;
    procedure WriteTo(Strings: TStrings; LastOne:Boolean; Level: Integer); override;
    property Items: TJSONObjects read FItems;
  published
  end;

  { TJSONArray }

  TJSONArray = class(TJSONBase)
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
        JSONBaseClass: TJSONBaseClass;
      end;

      TJSONClasses = class(TmnNamedObjectList<TJSONClass>)
      public
        procedure Add(AName: string; ABaseClass: TJSONBaseClass);
      end;

  protected
    JSONClasses: TJSONClasses;
    function GetObject(AJSONName: string; AJSONObjectClass: string; out AJSONObject: TObject): Boolean; override;
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

procedure TJSONValue.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
var
  Line: string;
begin
  inherited;
  Line := QuoteStr(Name, '"')+': '+QuoteStr(Value, '"');
  if not LastOne then
    Line := Line + ',';
  Strings.Add(Line);
end;

{ TJSONArray }

procedure TJSONArray.Created;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TJSONArray.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TJSONObject }

procedure TJSONObject.Created;
begin
  inherited;
  FItems := TJSONObjects.Create;
end;

destructor TJSONObject.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TJSONObject.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
var
  Itm: TJSONBase;
begin
  inherited;
  Strings.Add(QuoteStr(Name, '"') + ': {');
  for Itm in Items do
    Itm.WriteTo(Strings, itm = Items.Last , Level + 1);
  if not LastOne then
    Strings.Add('},')
  else
    Strings.Add('}');
end;

{ TJSONBase }

constructor TJSONBase.Create;
begin
  inherited Create;
end;

procedure TJSONBase.WriteTo(Strings: TStrings; LastOne: Boolean; Level: Integer);
begin
end;

{ TJSONClasses }

procedure TDOMJSONParser.TJSONClasses.Add(AName: string; ABaseClass: TJSONBaseClass);
var
  Item: TJSONClass;
begin
  Item := TJSONClass.Create;
  Item.Name := AName;
  Item.JSONBaseClass := ABaseClass;
  inherited Add(Item);
end;

{ TDOMJSONParser }

function TDOMJSONParser.GetObject(AJSONName: string; AJSONObjectClass: string; out AJSONObject: TObject): Boolean;
var
  Item: TJSONClass;
begin
  Item := JSONClasses.Find(AJSONObjectClass);
  if Item = nil then
    RaiseError('We do not have: ' + AJSONObjectClass);

  AJSONObject := Item.JSONBaseClass.Create;
  (AJSONObject as TJSONBase).Name := AJSONName;
  (AJSONObject as TJSONBase).JSONClass := AJSONObjectClass;
  Result := True;
end;

procedure TDOMJSONParser.SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin
  (AJSONObject as TJSONValue).Value := AJSONValue;
end;

procedure TDOMJSONParser.Created;
begin
  inherited Created;
  JSONClasses := TJSONClasses.Create;
  JSONClasses.Add('', TJSONValue);
  JSONClasses.Add('Value', TJSONValue);
  JSONClasses.Add('Object', TJSONObject);
  JSONClasses.Add('Array', TJSONArray);
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
begin
  with (Collector as TJSONCollectorValue) do
  begin
    case Control.Code of
      ctlOpenBlock:
      begin
        Post;
        if JSONObject = nil then //TODO: i dont like it
          (Parser as TJSONParser).GetObject(JSONName, 'Object', JSONObject);

        Parser.Push(TJSONCollectorElement.Create(Parser, JSONObject));

        if JSONObject = nil then
          RaiseError('Can not find object: ' + JSONName);
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

function TJSONParser.GetObject(AJSONName: string; AJSONObjectClass: string; out AJSONObject: TObject): Boolean;
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

end.
