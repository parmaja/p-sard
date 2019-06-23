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
  Classes, SysUtils,
  sardClasses, sardLexers, sardScanners, sardParsers;

type

  { TLineComment_Tokenizer }

  TLineComment_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;


  { TComment_Tokenizer }

  TComment_Tokenizer = class(TBufferedMultiLine_Tokenizer)
  public
    constructor Create; override;
    procedure SetToken(Text: string); override;
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

  TJSONParser = class;

  { TJSONScanner }

  TJSONScanner = class(TScanner)
  protected
    Parser: TJSONParser;
    FRoot: TPersistent;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(ARoot: TPersistent);
    property Root: TPersistent read FRoot;
  end;

  { TJSONParser }

  TJSONParser = class(TParser)
  protected
    LastControl: TSardControlID;
    Lexer: TLexer;
    procedure SetToken(Token: TSardToken); override;
    procedure SetOperator(AOperator: TSardOperator); override;
    procedure SetControl(AControl: TSardControl); override;
    procedure DoQueue;

  public
    constructor Create(ALexer: TLexer);
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  { TJSONScript }

  TJSONScript = class(TScript)
  public
    FRoot: TPersistent;
    Scanner: TJSONScanner;
    constructor Create(ARoot: TPersistent);
    destructor Destroy; override;
    procedure Compile(Lines: TStringList); override; overload;
    property Root: TPersistent read FRoot;
  end;

implementation

{ TLineComment_Tokenizer }

procedure TLineComment_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while IndexInStr(Column, Text) and (not Lexer.IsEOL(Text[Column])) do
    inc(Column);
  inc(Column);//Eat the EOF char
  Resume := False;
end;

function TLineComment_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := ScanText('//', Text, Column);
end;

{ TComment_Tokenizer }

constructor TComment_Tokenizer.Create;
begin
  inherited Create;
  OpenSymbol := '/*';
  CloseSymbol := '*/';
end;

procedure TComment_Tokenizer.SetToken(Text: string);
begin
  Lexer.Parser.SetToken(Token(ctlToken, typeComment, text));
end;

{ TJSONScript }

constructor TJSONScript.Create(ARoot: TPersistent);
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

procedure TJSONScanner.DoStart;
begin
  inherited DoStart;
end;

procedure TJSONScanner.DoStop;
begin
  inherited DoStop;
end;

constructor TJSONScanner.Create(ARoot: TPersistent);
begin
  inherited Create;
  FRoot := ARoot;
  Add(TJSONLexer.Create);
end;

{ TJSONParser }

procedure TJSONParser.SetToken(Token: TSardToken);
begin
  //here is the magic, we must find it in tokens detector to check if this id is normal id or is control or operator
  //by default it is id
  if (Token.TokenType <> typeIdentifier) or (not IsKeyword(Token.Value)) then
  begin
    (*
        We will send ; after } if we find a token
            x:= {
                    ...
                } <---------here not need to add ;
            y := 10;
    *)
    if (LastControl = ctlCloseBlock) then
    begin
      LastControl := ctlNone;//prevent loop
      SetControl(Lexer.Controls.GetControl(ctlEnd));
    end;
    Current.AddToken(Token);
    DoQueue();
    FActions := [];
    LastControl := ctlToken;
  end;
end;

procedure TJSONParser.SetOperator(AOperator: TSardOperator);
var
  o: TSardOperator;
begin
  inherited;
  o := AOperator;
  if (o = nil) then
    RaiseError('SetOperator not Operator');
  //Current.AddOperator(o);
  DoQueue();
  FActions := [];
  LastControl := ctlOperator;
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

constructor TJSONParser.Create(ALexer: TLexer);
begin
  inherited Create;
  Lexer := ALexer;
  //Push(TCollectorBlock.Create(Self, AStatements));
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
      Add(TSQString_Tokenizer.Create);
      Add(TDQString_Tokenizer.Create);
      Add(TEscape_Tokenizer.Create);
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
