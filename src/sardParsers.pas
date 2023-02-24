unit sardParsers;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey 
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

{$ifdef FPC}
{$define Windows}
{$endif}

interface

uses
  Classes, SysUtils,
  mnUtils,
  sardClasses;

type
  TSardControlID = (
    ctlNone,
    ctlStart, //Start parsing
    ctlStop, //Start parsing
    ctlDeclare, //Declare a class of object
    ctlAssign, //Assign to object/variable used as :=
//    ctlLet, //Same as assign but make it as lexical scope, this variable will be seen from descent/child objects
    ctlNext, //End Params, Comma
    ctlEnd, //End Statement Semicolon
    ctlOpenBlock, // {
    ctlCloseBlock, // }
    ctlOpenParams, // (
    ctlCloseParams, // )
    ctlOpenPreprocessor, //* <?
    ctlClosePreprocessor, //* ?>
    ctlOpenArray, // [
    ctlCloseArray, // ]
//    ctlObject
    ctlToken//* Token like Identifier, Keyword or Number
  );

  TSardTokenType = (
    typeNone,
    typeIdentifier,
    typeOperator,
    typeNumber,
    typeString,
    typeEscape, //Strings escape outside
    typeComment,
    typeColor,
    typeDateTime
  );

  { TsardToken }

  TSardToken = record
    Control: TsardControlID;
    TokenType: TSardTokenType;
    Value: string;
    procedure Init(AControl: TsardControlID; ATokenType: TSardTokenType; const AValue: string);
  end;

  TSardSymbolicObject = class abstract(TSardNamedObject)
  public
//    IsSymbol: Boolean; //when check is identifire do not use IsSymbol = false
  end;

  { TSardControl }

  TSardControl = class(TSardSymbolicObject)
  public
    Code: TSardControlID;
    Description: string;
    constructor Create(const AName: string; ACode: TsardControlID; const ADescription: string = '');
  end;

  { TSardControls }

  TSardControls = class(TSardNamedObjects<TSardControl>)
  public
    function FindControl(Code: TsardControlID): TSardControl;
    function GetControl(Code: TsardControlID): TSardControl;
    function Add(const AName: string; ACode: TsardControlID; const ADescription: string = ''): TSardControl;
  end;

  TParser = class;
  TLexer = class;
  TScanner = class;
  TCollector = class;
  TController = class;

  {**
  *
  *   Tokenizer
  *   Small object scan one type of token
  *
  *}

  { TTokenizer }

  TTokenizer = class abstract(TSardObject)
  private
    FLexer: TLexer;
    procedure SetLexer(const Value: TLexer);
  protected
    procedure LexerChanged; virtual;
    //Return true if it done, next will auto detect it detect
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); virtual; abstract;
    function Accept(const Text: string; Column: Integer): Boolean; virtual; abstract;
    procedure Finish; virtual;
    //This function call when switched to it
    procedure Switched;
    procedure SetToken(Token: TSardToken); virtual;
  public
    property Lexer: TLexer read FLexer write SetLexer;
    constructor Create; virtual;
  end;

  TSardTokenizerClass = class of TTokenizer;

  { TLexer }

  TLexer = class abstract(TSardObjects<TTokenizer>)
  private
    FParser: TParser;
    FScanner: TScanner;
    FCurrent: TTokenizer;
    FControls: TSardControls;
    FControlsOpens: TSysCharSet;
    procedure SetParser(AValue: TParser);
  protected
    function DetectTokenizer(const Text: String; Column: integer): TTokenizer;
    procedure SwitchTokenizer(NextTokenizer: TTokenizer);
    function FindClass(AClass: TSardTokenizerClass): TTokenizer;
    function SelectTokenizer(AClass: TSardTokenizerClass): TTokenizer;
    procedure Added(Item: TTokenizer); override;
  public
    TrimToken: Boolean; //ommit send open and close tags when SetToken
    constructor Create; virtual;
    destructor Destroy; override;

    function IsEOL(vChar: Char): Boolean; virtual; abstract;
    function IsWhiteSpace(const vChar: Char; vOpen: Boolean = true): Boolean; virtual; abstract;
    function IsControl(vChar: Char): Boolean; virtual;
    function IsNumber(const vChar: Char; vOpen: Boolean = true): Boolean; virtual; abstract;

//    function IsKeyword(Keyword: string): Boolean;
    function IsIdentifier(const vChar: Char; vOpen: Boolean = true): Boolean;

    procedure ScanLine(const Text: String; Line: Integer; var Column: Integer);
    procedure Start;
    procedure Stop;

    property Scanner: TScanner read FScanner;
    property Current: TTokenizer read FCurrent;
    property Parser: TParser read FParser write SetParser;
    property Controls: TSardControls read FControls;
  end;

  { TControl_Tokenizer }

  TControl_Tokenizer = class(TTokenizer)
  protected
    procedure LexerChanged; override;
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  public
    Control: TSardControl;
    constructor Create(const AName: string; ACode: TsardControlID; const ADescription: string = ''); overload;
    constructor Create(AControl: TSardControl = nil); overload;
  end;

  { TController }

  TController = class abstract(TSardObject)
  private
    FCollector: TCollector;
  public
    constructor Create(ACollector: TCollector);
    procedure SetControl(Control: TSardControl); virtual; abstract;
    property Collector: TCollector read FCollector;
  end;

  { TCollector }

  TCollector = class abstract(TSardObject)
  private
    FController: TController;
    FParser: TParser;
  protected
    procedure InternalPost; virtual;
    function CreateController: TController; virtual;
    procedure DoToken(Token: TSardToken); virtual; abstract;
    procedure DoControl(AControl: TSardControl); virtual; abstract;
  public
    constructor Create(AParser: TParser);
    destructor Destroy; override;

    procedure Reset; virtual; abstract;

    procedure SetToken(Token: TSardToken);
    procedure SetControl(AControl: TSardControl);
    function IsInitial: Boolean; virtual;

    property Controller: TController read FController;
    property Parser: TParser read FParser;
  end;

  TParserAction = (
    paPop,  //pop collector
    paPass //Pass the control to upper collector
  );

  TParserActions = set of TParserAction;

  { TParser }

  TParser = class(TSardStack<TCollector>)
  protected
    FActions: TParserActions;
    FNextCollector: TCollector;
  public
    constructor Create(OwnItems: Boolean); override;
    procedure Start; virtual;
    procedure Stop; virtual;
    function IsKeyword(const AIdentifier: string): Boolean; virtual;

    procedure SetToken(Token: TSardToken); virtual;
    procedure SetControl(AControl: TSardControl); virtual;
    procedure SetAction(AActions: TParserActions = []; ANextCollector: TCollector = nil); virtual;

    property Actions: TParserActions read FActions;
    property NextCollector: TCollector read FNextCollector;
  end;

  TParserClass = class of TParser;

  { TScanner }

  TScanner = class abstract(TSardObjects<TLexer>)
  private
    FActive: Boolean;
    FCharset: string;
    FLine: Integer;
    FVer: string;
    FCurrent: TLexer;
    FParser: TParser;
  protected
    procedure Added(Item: TLexer); override;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    function CreateParser: TParser; virtual; abstract;
  public
    procedure ScanChunk(const Text: String; Line: Integer);
    procedure Scan(Lines: TStringList); overload;
    procedure Scan(const Text: string); overload;
    procedure Start;
    procedure Stop;

    property Active: Boolean read FActive;
    property Ver: string read FVer;
    property Charset: string read FCharset;
    property Line: Integer read FLine;
    property Current: TLexer read FCurrent;
    property Parser: TParser read FParser;
  end;

function Token(AControl: TsardControlID; ATokenType: TSardTokenType; const AValue: string): TSardToken;

implementation

uses
  StrUtils;

{ TScanner }

procedure TScanner.Added(Item: TLexer);
begin
  inherited;
  Item.FScanner := Self;
end;

procedure TScanner.DoStart;
begin
end;

procedure TScanner.DoStop;
begin
end;

procedure TScanner.Scan(Lines: TStringList);
var
  i: Integer;
begin
  Start;
  for i := 0 to Lines.Count -1 do
  begin
    ScanChunk(Lines[i]+#13, i + 1); //DO not use TRIM
  end;
  Stop;
end;

procedure TScanner.Scan(const Text: string);
begin
  Start;
  ScanChunk(Text, 0);
  Stop;
end;

procedure TScanner.ScanChunk(const Text: String; Line: Integer);
var
  Column: Integer;
begin
  if (not Active) then
    RaiseError('Should be started first');
  FLine := Line;
  Column := 1;
  //Column := 0; when convert it to C, D
  Current.ScanLine(Text, Line, Column);
end;

procedure TScanner.Start;
begin
  if Active then
    RaiseError('Already opened');
  if Count = 0 then
    RaiseError('There is no lexers added');
  FActive := True;
  FCurrent := Self[0]; //First one

  FParser := CreateParser;
  FCurrent.Parser := Parser;
  FCurrent.Start;
  DoStart;
end;

procedure TScanner.Stop;
begin
  if not Active then
    RaiseError('Already closed');
  DoStop;
  Current.Stop;
  Current.Parser := nil;
  FreeAndNil(FParser);
  FCurrent := nil;
  FActive := False;
end;

{ TLexer }

procedure TLexer.SetParser(AValue: TParser);
begin
  if FParser <> AValue then
  begin
    FParser := AValue;
  end;
end;

function TLexer.DetectTokenizer(const Text: String; Column: integer): TTokenizer;
var
  itm: TTokenizer;
begin
  Result := nil;
  if (Column > Length(Text)) then //>= in C,D
  begin
    //do i need to switchTokenizer?
    //return null; //no tokenizer for empty line or EOL
    //Result := nil; nothing to do already nil
  end
  else
  begin
    for itm in Self do
    begin
      if itm.Accept(Text, Column) then
      begin
        Result := itm;
        break;
      end;
    end;
    if Result = nil then
      RaiseError('Tokenizer not found: ' + Text[Column]);
  end;
  SwitchTokenizer(Result);
end;

procedure TLexer.SwitchTokenizer(NextTokenizer: TTokenizer);
begin
  if (FCurrent <> NextTokenizer) then
  begin
    FCurrent := NextTokenizer;
    if FCurrent <> nil then
      FCurrent.Switched;
  end;
end;

function TLexer.FindClass(AClass: TSardTokenizerClass): TTokenizer;
var
  itm: TTokenizer;
begin
  Result := nil;
  for itm in Self do
  begin
    if itm.ClassType = AClass then
    begin
      Result := itm;
      break;
    end;
  end;
end;

function TLexer.SelectTokenizer(AClass: TSardTokenizerClass): TTokenizer;
begin
  Result := FindClass(AClass);
  if Result = nil then
    RaiseError('Tokenizer not found');
  SwitchTokenizer(Result);
end;

procedure TLexer.Added(Item: TTokenizer);
begin
  inherited;
  Item.Lexer := Self;
end;

constructor TLexer.Create;
begin
  inherited Create(true);
  FControls := TSardControls.Create;
  TrimToken := True;
end;

destructor TLexer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FControls);
end;

{function TLexer.IsKeyword(Keyword: string): Boolean;
begin
  Result := false;
end;}

function TLexer.IsControl(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, FControlsOpens);
end;

function TLexer.IsIdentifier(const vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := not isWhiteSpace(vChar) and not IsControl(vChar) and not IsEOL(vChar);
  if (vOpen) then
      Result := Result and not IsNumber(vChar, vOpen);
end;

procedure TLexer.ScanLine(const Text: String; Line: Integer; var Column: Integer);
var
  len: Integer;
  Resume: Boolean;
  OldColumn: Integer;
  OldTokenizer: TTokenizer;
begin
  len := Length(Text);
  Resume := False;
  while (Column <= len) do
  begin
    OldColumn := Column;
    OldTokenizer := Current;
    try
      if Current = nil then
        DetectTokenizer(Text, Column)
      else
        Resume := True;

      if Current = nil then
        raise ESardParserException.Create('Tokenizer not detected', Line, Column);

      Current.Scan(Text, Column, Column, Resume);

      if not Resume then
        SwitchTokenizer(nil);

      if ((OldColumn = Column) and (OldTokenizer = Current)) then
        RaiseError('Forever loop with: ' + Current.ClassName); //TODO: be careful here
    except
      on E: Exception do
      begin
        raise ESardParserException.Create(E.Message, Line, Column);
      end;
    end;
  end;
end;

procedure TLexer.Start;
begin
  if Parser = nil then
    RaiseError('Parser should be not null');
  Parser.Start;
end;

procedure TLexer.Stop;
begin
  if Current <> nil then
    Current.Finish;
  Parser.Stop;
end;

{ TControl_Tokenizer }

function TControl_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  if Control = nil then
    Result := Lexer.IsControl(Text[Column])
  else
  begin
    Result := Control.IsOpenBy(Text[Column]);
    if Result then
    begin
      Column := Column + Length(Control.Name);
      Lexer.Parser.SetControl(Control);
    end;
  end;
end;

constructor TControl_Tokenizer.Create(AControl: TSardControl);
begin
  inherited Create;
  Control := AControl;
end;

procedure TControl_Tokenizer.LexerChanged;
begin
  inherited;
  if Control <> nil then
    Lexer.FControlsOpens := Lexer.FControlsOpens + [Control.Name[1]];
end;

constructor TControl_Tokenizer.Create(const AName: string; ACode: TsardControlID; const ADescription: string);
begin
  Create(TSardControl.Create(AName, ACode, ADescription));
end;

procedure TControl_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
var
  AControl: TSardControl;
begin
  if Control = nil then
  begin
    AControl := Lexer.Controls.Scan(Text, Column);
    if AControl <> nil then
      Column := Column + Length(AControl.Name)
    else
      RaiseError('Unkown control started with ' + Text[Started]);
    Lexer.Parser.SetControl(AControl);
    Resume := False;
  end
  else
  begin
    Column := Column + Length(Control.Name);
    Lexer.Parser.SetControl(Control);
    Resume := False;
  end;
end;

{ TSardControls }

function TSardControls.FindControl(Code: TsardControlID): TSardControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Code = Code then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TSardControls.GetControl(Code: TsardControlID): TSardControl;
begin
  if Count = 0 then
    raise EsardException.Create('No controls is added');
  Result := FindControl(Code);
  if Result = nil then
    raise EsardException.Create('Control not found');
end;

function TSardControls.Add(const AName: string; ACode: TsardControlID; const ADescription: string): TSardControl;
begin
  if FindControl(ACode) <> nil then
    RaiseError('Control already exists');
  Result := TSardControl.Create(AName, ACode, ADescription);
  inherited Add(Result)
end;

{ TTokenizer }

procedure TTokenizer.Finish;
begin
end;

procedure TTokenizer.LexerChanged;
begin
end;

procedure TTokenizer.SetLexer(const Value: TLexer);
begin
  FLexer := Value;
  LexerChanged;
end;

procedure TTokenizer.SetToken(Token: TSardToken);
begin
  Lexer.Parser.SetToken(Token);
end;

procedure TTokenizer.Switched;
begin
   //Maybe reseting buffer or something
end;

constructor TTokenizer.Create;
begin
  inherited Create;
end;

{ TSardControl }

constructor TSardControl.Create(const AName: string; ACode: TsardControlID; const ADescription: string);
begin
  inherited Create;
  Name := AName;
  Code := ACode;
  Description := ADescription;
end;

{ TSardToken }

procedure TSardToken.Init(AControl: TsardControlID; ATokenType: TSardTokenType; const AValue: string);
begin
  Control := AControl;
  TokenType := ATokenType;
  Value := AValue;
end;

function Token(AControl: TsardControlID; ATokenType: TSardTokenType; const AValue: string): TSardToken;
begin
  with Result do
  begin
    Control := AControl;
    TokenType := ATokenType;
    Value := AValue;
  end;
end;

{ TParser }

function TParser.IsKeyword(const AIdentifier: string): Boolean;
begin
  Result := False;
end;

procedure TParser.SetToken(Token: TSardToken);
begin

end;

procedure TParser.SetControl(AControl: TSardControl);
begin

end;

procedure TParser.Start;
begin
  if Current = nil then
    RaiseError('At last you need one collector push');
end;

procedure TParser.Stop;
begin

end;

procedure TParser.SetAction(AActions: TParserActions; ANextCollector: TCollector);
begin
  FActions := AActions;
  FNextCollector := ANextCollector;
end;

constructor TParser.Create(OwnItems: Boolean);
begin
  inherited;
end;

{ TController }

constructor TController.Create(ACollector: TCollector);
begin
  inherited Create;
  FCollector := ACollector;
end;

{ TCollector }

procedure TCollector.SetControl(AControl: TSardControl);
begin
  {$ifdef VERBOSE}
  if AControl.Description <> '' then
    WriteLn('Control: ' + AControl.Description)
  else
    WriteLn('Control: ' + AControl.Name);
  {$endif}
  DoControl(AControl);
  if Controller <> nil then
    Controller.SetControl(AControl);
end;

function TCollector.IsInitial: Boolean;
begin
  Result := False;
end;

destructor TCollector.Destroy;
begin
  inherited;
  FreeAndNil(FController);
end;

procedure TCollector.SetToken(Token: TSardToken);
begin
  {$ifdef VERBOSE}
   WriteLn('Token: ' + Token.Value);
  {$endif}
  DoToken(Token);
end;

procedure TCollector.InternalPost;
begin
end;

function TCollector.CreateController: TController;
begin
  Result := nil;
end;

constructor TCollector.Create(AParser: TParser);
begin
  Inherited Create;
  FParser := AParser;
  FController := CreateController;
  Reset;
end;

end.
