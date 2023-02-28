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
  mnUtils, mnStreams,
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

  TSardControl = record
  public
    Name: string;
    Code: TSardControlID;
    Description: string;
    constructor Create(const AName: string; ACode: TsardControlID; const ADescription: string = '');
  end;

  { TSardControls }

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
    function Accept(const Text: string; var Column: Integer): Boolean; virtual; abstract;
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
    procedure SetParser(AValue: TParser);
  protected
    function DetectTokenizer(const Text: String; Column: integer): TTokenizer;
    procedure SwitchTokenizer(NextTokenizer: TTokenizer);
    function FindClass(AClass: TSardTokenizerClass): TTokenizer;
    function SelectTokenizer(AClass: TSardTokenizerClass): TTokenizer;
    procedure Added(Item: TTokenizer); override;
  public
    WhiteSpaces: TSysCharSet;
    Symbols: TSysCharSet; //Symbols is chars that break scanning identifier, collected from Controls, Number
    TrimToken: Boolean; //ommit send open and close tags when SetToken
    constructor Create; virtual;
    destructor Destroy; override;

    function IsEOL(vChar: Char): Boolean; inline;
    function IsWhiteSpace(const vChar: Char; vOpen: Boolean = true): Boolean; inline;
    function IsSymbol(vChar: Char): Boolean; inline;

    function IsNumber(const vChar: Char; vOpen: Boolean = true): Boolean; virtual; abstract;

    function IsIdentifier(const vChar: Char; vOpen: Boolean = true): Boolean;

    procedure ScanLine(const Text: String; Line: Integer; var Column: Integer);
    procedure Start;
    procedure Stop;

    property Scanner: TScanner read FScanner;
    property Current: TTokenizer read FCurrent;
    property Parser: TParser read FParser write SetParser;
  end;

  { TControl_Tokenizer }

  TControl_Tokenizer = class(TTokenizer)
  protected
    procedure LexerChanged; override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
  public
    Control: TSardControl;
    constructor Create(const AName: string; ACode: TsardControlID; const ADescription: string = ''); overload;
    constructor Create(AControl: TSardControl); overload;
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

  TCollector = class abstract(TSardStackObject)
  private
    FController: TController;
    FParser: TParser;
  protected
    procedure InternalPost; virtual;
    function CreateController: TController; virtual;
    procedure DoToken(Token: TSardToken); virtual; abstract;
    procedure DoControl(AControl: TSardControl); virtual; abstract;
  public
    constructor Create(AParser: TParser); overload;
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
    FLine: Integer;
    FActions: TParserActions;
    FNextCollector: TCollector;
    procedure Queue; virtual;
  public
    ControlStart: TSardControl;
    ControlStop: TSardControl;
    constructor Create(OwnItems: Boolean); override;
    procedure Start; virtual;
    procedure Stop; virtual;
    function IsKeyword(const AIdentifier: string): Boolean; virtual;

    procedure SetToken(Token: TSardToken); virtual;
    procedure SetControl(AControl: TSardControl); virtual;
    procedure SetAction(AActions: TParserActions = []; ANextCollector: TCollector = nil); virtual;

    property Actions: TParserActions read FActions;
    property NextCollector: TCollector read FNextCollector;

    property Line: Integer read FLine;
  end;

  TParserClass = class of TParser;

  { TScanner }

  TScanner = class abstract(TSardObjects<TLexer>)
  private
    FActive: Boolean;
    FCharset: string;
    FVer: string;
    FCurrent: TLexer;
    FParser: TParser;
  protected
    procedure Added(Item: TLexer); override;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    function CreateParser: TParser; virtual; abstract;
  public
    procedure ScanLine(const Text: String);
    procedure Scan(Lines: TStringList); overload;
    procedure Scan(const Text: string); overload;
    procedure Scan(const Stream: TStream); overload;
    procedure ScanFile(const FileName: string); overload;
    procedure Start;
    procedure Stop;

    property Active: Boolean read FActive;
    property Ver: string read FVer;
    property Charset: string read FCharset;
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
    ScanLine(Lines[i]); //DO not use TRIM
  end;
  Stop;
end;

procedure TScanner.Scan(const Text: string);
begin
  Start;
  ScanLine(Text);
  Stop;
end;

procedure TScanner.Scan(const Stream: TStream);
var
  w: TmnWrapperStream;
begin
  Start;
  w := TmnWrapperStream.Create(Stream, False);
  try
    while not (cloRead in w.Done) do
    begin
      ScanLine(w.ReadLine(False));
    end;
    Stop;
  finally
    w.Free;
  end;
end;

procedure TScanner.ScanFile(const FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    Scan(fs);
  finally
    fs.Free;
  end;
end;

procedure TScanner.ScanLine(const Text: String);
var
  Column: Integer;
begin
  if (not Active) then
    RaiseError('Should be started first');
  Inc(Parser.FLine);
  Column := 1;
  //Column := 0; when convert it to C, D
  Current.ScanLine(Text, Parser.Line, Column);
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
  i: Integer;
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
    for i := 0 to Count -1 do
    begin
      if Items[i].Accept(Text, Column) then
      begin
        Result := Items[i];
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
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
  begin
    if Items[i].ClassType = AClass then
    begin
      Result := Items[i];
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
  WhiteSpaces := [' ', #8, #9, #10, #13];
  TrimToken := True;
end;

destructor TLexer.Destroy;
begin
  inherited Destroy;
end;

function TLexer.IsEOL(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sEOL);
end;

function TLexer.IsIdentifier(const vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := not isWhiteSpace(vChar) and not IsSymbol(vChar) and not IsEOL(vChar);
  if (vOpen) then
      Result := Result and not IsNumber(vChar, vOpen);
end;

function TLexer.IsSymbol(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, Symbols);
end;

function TLexer.IsWhiteSpace(const vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := CharInSet(vChar, WhiteSpaces);
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

function TControl_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanString(Control.Name, Text, Column);
end;

constructor TControl_Tokenizer.Create(AControl: TSardControl);
begin
  inherited Create;
  Control := AControl;
end;

procedure TControl_Tokenizer.LexerChanged;
begin
  inherited;
  if not (CharInSet(Control.Name[1], Lexer.Symbols)) then
    Lexer.Symbols := Lexer.Symbols + [Control.Name[1]];
end;

constructor TControl_Tokenizer.Create(const AName: string; ACode: TsardControlID; const ADescription: string);
begin
  Create(TSardControl.Create(AName, ACode, ADescription));
end;

procedure TControl_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Column := Column + Length(Control.Name);
  Lexer.Parser.SetControl(Control);
  Resume := False;
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

procedure TParser.Queue;
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

procedure TParser.SetToken(Token: TSardToken);
begin
  Current.SetToken(Token);
  Queue;
  FActions := [];
end;

procedure TParser.SetControl(AControl: TSardControl);
begin
  if Current = nil then
    RaiseError('There is no current collector');
  Current.SetControl(AControl);
  while true do
  begin
    Queue;
    if (paPass in Actions) then
    begin
      FActions := FActions - [paPass];
      Current.SetControl(AControl)
    end
    else
      break;
  end;
end;

procedure TParser.Start;
begin
  SetControl(ControlStart);
end;

procedure TParser.Stop;
begin
  try
    if Current <> nil then //not already finished
      SetControl(ControlStop);
  except
    on E: Exception do
      RaiseError(E.Message, Line)
  end;
end;

procedure TParser.SetAction(AActions: TParserActions; ANextCollector: TCollector);
begin
  FActions := AActions;
  FNextCollector := ANextCollector;
end;

constructor TParser.Create(OwnItems: Boolean);
begin
  inherited;
  ControlStart := TSardControl.Create('', ctlStart, '');
  ControlStop := TSardControl.Create('', ctlStop, '');
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
  Create;
  FParser := AParser;
  FController := CreateController;
  Reset;
end;

end.
