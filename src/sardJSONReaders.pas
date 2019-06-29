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

  { TJSONControllerName }

  TJSONControllerName = class(TJSONController)
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

  TJSONControllerValue = class(TJSONController)
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

  { TJSONParser }

  TJSONParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    procedure DoQueue;
    //AJSONObjectClass is not used in JSON format
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

  { TDOMJSONParser }

  TDOMJSONParser = class(TJSONParser)
  protected
    procedure SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string); override;
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

{ TDOMJSONParser }

procedure TDOMJSONParser.SetObjectValue(AJSONObject: TObject; AJSONName: string; AJSONValue: string);
begin
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
        if (Parser as TJSONParser).GetObject(JSONName, '', AJSONObject) then
          Parser.Push(TJSONCollectorName.Create(Parser, JSONObject))
        else
          RaiseError('Can not find object: ' + JSONName);
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
    (Parser as TJSONParser).SetObjectValue(JSONObject, JSONName, DequoteStr(Token.Value));
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

end.
