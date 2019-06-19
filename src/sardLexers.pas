unit sardLexers;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  mnUtils,
  mnClasses, sardClasses;

type
  TsardControlID = (
    ctlNone,
    ctlToken,//* Token like Identifier, Keyword or Number
    ctlOperator,//
    ctlStart, //Start parsing
    ctlStop, //Start parsing
    ctlDeclare, //Declare a class of object
    ctlAssign, //Assign to object/variable used as :=
//    ctlLet, //Same as assign in the initial but is equal operator if not in initial statment used to be =
    ctlNext, //End Params, Comma
    ctlEnd, //End Statement Semicolon
    ctlOpenBlock, // {
    ctlCloseBlock, // }
    ctlOpenParams, // (
    ctlCloseParams, // )
    ctlOpenPreprocessor, //* <?
    ctlClosePreprocessor, //* ?>
    ctlOpenArray, // [
    ctlCloseArray // ]
  );

  TSardTokenType = (
    typeNone,
    typeIdentifier,
    typeNumber,
    typeColor,
    typeString,
    typeEscape, //Maybe Strings escape
    typeComment
  );

  { TsardToken }

  TSardToken = record
    Control: TsardControlID;
    TokenType: TSardTokenType;
    Value: string;
    procedure Init(AControl: TsardControlID; ATokenType: TSardTokenType; AValue: string);
  end;

  TSardSymbolicObject = class abstract(TSardNamedObject)
  public
    IsSymbol: Boolean; //when check is identifire do not use IsSymbol = false
  end;

  { TSardControl }

  TSardControl = class(TSardSymbolicObject)
  public
    Code: TSardControlID;
    Level: Integer;
    Description: string;
    constructor Create(AName: string; ACode: TsardControlID);
  end;

  { TSardControls }

  TSardControls = class(TSardNamedObjects<TSardControl>)
  public
    function FindControl(Code: TsardControlID): TSardControl;
    function GetControl(Code: TsardControlID): TSardControl;
    function Add(AName: string; ACode: TsardControlID): TSardControl;
  end;

  TSardAssociative = (asLeft, asRight);//not yet

  TSardOperator = class(TSardSymbolicObject)
  public
    Associative: TSardAssociative;
    //Precedence: Integer; //TODO it is bad idea, we need more intelligent way to define the power level of operators
    Title: string;
    Description: string;
  end;

  { TSardOperators }

  TSardOperators = class(TSardNamedObjects<TSardOperator>)
  public
    function FindByTitle(const Title: string): TSardOperator;
  end;

  TSardSymbol = class(TSardSymbolicObject)
  public
  end;

  TSardSymbols = class(TSardNamedObjects<TSardSymbol>)
  public
  end;

  IParser = interface(IInterface)
    //isKeyword call in setToken if you proceesed it return false
    //You can proceess as to setControl or setOperator
    function IsKeyword(AIdentifier: string): Boolean;

    procedure SetToken(AToken: TSardToken);
    procedure SetControl(AControl: TSardControl);
    procedure SetOperator(AOperator: TSardOperator);
    procedure SetWhiteSpaces(AWhitespaces: string);

    procedure Start;
    procedure Stop;

  end;

  TLexer = class;
  TScanner = class;

  {**
  *
  *   Tokenizer
  *   Small object scan one type of token
  *
  *}

  { TTokenizer }

  TTokenizer = class(TSardObject)
  private
    FLexer: TLexer;
  protected
    //Return true if it done, next will auto detect it detect
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); virtual; abstract;
    function Accept(Text: string; Column: Integer): Boolean; virtual; abstract;
    //This function call when switched to it
    procedure Switched;
  public
    property Lexer: TLexer read FLexer;
    //constructor Create(ALexer: TLexer); virtual;
    constructor Create; virtual;
  end;

  TSardTokenizerClass = class of TTokenizer;

  { TMultiLine_Tokenizer }

  TMultiLine_Tokenizer = class abstract(TTokenizer)
  protected
    OpenSymbol: string;
    CloseSymbol: string;
    procedure Finish; virtual; abstract;
    procedure Collect(Text: string); virtual; abstract;

    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TBufferedMultiLine_Tokenizer }

  TBufferedMultiLine_Tokenizer = class(TMultiLine_Tokenizer)
  private
    Buffer: string;
  protected
    procedure SetToken(Text: string); virtual; abstract;
    procedure Collect(Text: string); override;
    procedure Finish; override;
  end;

  { TString_Tokenizer }

  TString_Tokenizer = class abstract(TBufferedMultiLine_Tokenizer)
  public
    procedure SetToken(Text: string); override;
  end;

  { TLexer }

  TLexer = class(TSardObjects<TTokenizer>)
  private
    FParser: IParser;
    FScanner: TScanner;
    FCurrent: TTokenizer;
    FControls: TSardControls;
    FOperators: TSardOperators;
    FSymbols: TSardSymbols;
    procedure SetParser(AValue: IParser);
  protected
    function DetectTokenizer(Text: String; Column: integer): TTokenizer;
    procedure SwitchTokenizer(NextTokenizer: TTokenizer);
    function FindClass(AClass: TSardTokenizerClass): TTokenizer;
    function SelectTokenizer(AClass: TSardTokenizerClass): TTokenizer;
    procedure Added(Item: TTokenizer); override;
  public
    TrimSymbols: Boolean; //ommit send open and close tags when setToken
    constructor Create; virtual;
    destructor Destroy; override;

    function IsEOL(vChar: Char): Boolean; virtual; abstract;
    function IsWhiteSpace(vChar: char; vOpen: Boolean = true): Boolean; virtual; abstract;
    function IsSymbol(vChar: Char): Boolean; virtual; abstract;
    function IsControl(vChar: Char): Boolean; virtual; abstract;
    function IsOperator(vChar: Char): Boolean; virtual; abstract;
    function IsNumber(vChar: Char; vOpen: Boolean = true): Boolean; virtual; abstract;

//    function IsKeyword(Keyword: string): Boolean;
    function IsIdentifier(vChar: Char; vOpen: Boolean = true): Boolean;

    procedure ScanLine(Text: String; Line: Integer; var Column: Integer);
    procedure Start;
    procedure Stop;

    property Scanner: TScanner read FScanner;
    property Current: TTokenizer read FCurrent;
    property Parser: IParser read FParser write SetParser;
    property Symbols: TSardSymbols read FSymbols;
    property Controls: TSardControls read FControls;
    property Operators: TSardOperators read FOperators;
  end;

  { TScanner }

  TScanner = class abstract(TSardObjects<TLexer>)
  private
    FActive: Boolean;
    FCharset: string;
    FLine: Integer;
    FVer: string;
    FLexer: TLexer;
  protected
    procedure Added(Item: TLexer); override;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    procedure ScanLine(Text: String; Line: Integer);
    procedure Scan(Lines: TStringList); overload;
    procedure Start;
    procedure Stop;

    property Active: Boolean read FActive;
    property Ver: string read FVer;
    property Charset: string read FCharset;
    property Line: Integer read FLine;
    property Lexer: TLexer read FLexer;
  end;

function Token(AControl: TsardControlID; ATokenType: TSardTokenType; AValue: string): TSardToken;

implementation

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

procedure TScanner.ScanLine(Text: String; Line: Integer);
var
  Column: Integer;
begin
  if (not Active) then
    RaiseError('Should be started first');
  FLine := Line;
  Column := 1;
  //Column := 0; when convert it to C, D
  Lexer.ScanLine(Text, Line, Column);
end;

procedure TScanner.Scan(Lines: TStringList);
var
  i: Integer;
begin
  Start;
  for i := 0 to Lines.Count -1 do
  begin
    ScanLine(Lines[i], i + 1);
  end;
  Stop;
end;

procedure TScanner.Start;
begin
  if Active then
    RaiseError('Already opened');
  FActive := True;
  FLexer := Self[0]; //First one
  DoStart;
end;

procedure TScanner.Stop;
begin
  if not Active then
    RaiseError('Already closed');
  DoStop;
  FLexer := nil;
  FActive := False;
end;

{ TMultiLine_Tokenizer }

procedure TMultiLine_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  if not Resume then
  begin
    Column := Column + Length(openSymbol);
  end;

  while (IndexInStr(Column, Text)) do //Use < instead of <= in C, D
  begin
    if ScanCompare(CloseSymbol, Text, Column) then
    begin
      if (not Lexer.TrimSymbols) then
        Column := Column + Length(CloseSymbol);
      Collect(SliceText(Text, Started, Column));
      if (Lexer.TrimSymbols) then
          Column := Column + Length(CloseSymbol);
      Finish;
      Resume := False;
      exit;
    end;
    Inc(Column);
  end;
  collect(text[Started..Column]);
  Resume := true;
end;

function TMultiLine_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := ScanText(openSymbol, text, column);
end;

{ TBufferedMultiLine_Tokenizer }

procedure TBufferedMultiLine_Tokenizer.Collect(Text: string);
begin
  Buffer := Buffer + Text;
end;

procedure TBufferedMultiLine_Tokenizer.Finish;
begin
  SetToken(Buffer);
  Buffer := '';
end;

procedure TString_Tokenizer.SetToken(Text: string);
begin
  Lexer.Parser.SetToken(Token(ctlToken, typeString, Text));
end;

{ TLexer }

procedure TLexer.SetParser(AValue: IParser);
begin
  if FParser <> AValue then
  begin
    FParser := AValue;
  end;
end;

function TLexer.DetectTokenizer(Text: String; Column: integer): TTokenizer;
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
  Item.FLexer := Self;
end;

constructor TLexer.Create;
begin
  inherited Create(true);
  FControls := TSardControls.Create;
  FOperators := TSardOperators.Create;
  FSymbols := TSardSymbols.Create;
  TrimSymbols := True;
end;

destructor TLexer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FControls);
  FreeAndNil(FOperators);
  FreeAndNil(FSymbols);
end;
{
function TLexer.IsKeyword(Keyword: string): Boolean;
begin
  Result := false;
end;}

function TLexer.IsIdentifier(vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := not isWhiteSpace(vChar) and not IsControl(vChar) and not IsOperator(vChar) and not IsSymbol(vChar);
  if (vOpen) then
      Result := Result and not IsNumber(vChar, vOpen);
end;

procedure TLexer.ScanLine(Text: String; Line: Integer; var Column: Integer);
var
  len: Integer;
  Resume: Boolean;
  OldColumn: Integer;
  OldTokenizer: TTokenizer;
begin
  len := Length(Text);
  Resume := false;
  while (Column <= len) do
  begin
    OldColumn := Column;
    OldTokenizer := Current;
    try
      if Current = nil then
        DetectTokenizer(Text, Column)
      else
        Resume := True;

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
  Parser.Start;
end;

procedure TLexer.Stop;
begin
  Parser.Stop;
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

function TSardControls.Add(AName: string; ACode: TsardControlID): TSardControl;
begin
  Result := TSardControl.Create(AName, ACode);
  inherited Add(Result)
end;

{ TSardOperators }

function TSardOperators.FindByTitle(const Title: string): TSardOperator;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Title, Title) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

{ TTokenizer }

procedure TTokenizer.Switched;
begin
   //Maybe reseting buffer or something
end;

constructor TTokenizer.Create;
begin
  inherited Create;
//  FLexer := ALexer;
end;

{ TSardControl }

constructor TSardControl.Create(AName: string; ACode: TsardControlID);
begin
  inherited Create;
  Name := AName;
  Code := ACode;
end;

{ TSardToken }

procedure TSardToken.Init(AControl: TsardControlID; ATokenType: TSardTokenType; AValue: string);
begin
  Control := AControl;
  TokenType := ATokenType;
  Value := AValue;
end;

function Token(AControl: TsardControlID; ATokenType: TSardTokenType; AValue: string): TSardToken;
begin
  with Result do
  begin
    Control := AControl;
    TokenType := ATokenType;
    Value := AValue;
  end;
end;

end.
