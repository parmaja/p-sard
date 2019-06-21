unit sardScanners;
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
  mnUtils, SysUtils,
  sardClasses, sardObjects, sardLexers, sardOperators;

const
  sEOL = [#0, #13, #10];

  sEscape = '\';

  //sColorOpenChars = ['#'];
  //sColorChars = sColorOpenChars + ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

type
  { TWhitespace_Tokenizer }

  TWhitespace_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TIdentifier_Tokenizer }

  TIdentifier_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TNumber_Tokenizer }

  TNumber_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TControl_Tokenizer }

  TControl_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TOperator_Tokenizer }

  TOperator_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TLineComment_Tokenizer }

  TLineComment_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  { TBlockComment_Tokenizer }

  TBlockComment_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  end;

  //Comment object {* *}

  { TComment_Tokenizer }

  TComment_Tokenizer = class(TBufferedMultiLine_Tokenizer)
  public
    constructor Create; override;
    procedure SetToken(Text: string); override;
  end;

  {* Single Quote String *}

  { TSQString_Tokenizer }

  TSQString_Tokenizer = class(TString_Tokenizer)
  public
    constructor Create; override;
  end;

  {* Double Quote String *}

  { DQString_Tokenizer }

  TDQString_Tokenizer = class(TString_Tokenizer)
  public
    constructor Create; override;
  end;

  { TEscape_Tokenizer }

  TEscape_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(Text: string; Column: Integer): Boolean; override;
  public
  end;

const
  sWhitespace = sEOL + [' ', #8];
  sNumberOpenChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  sNumberChars = sNumberOpenChars + ['.', 'x', 'h', 'a', 'b', 'c', 'd', 'e', 'f'];
  sSymbolChars = ['"', '''', '\']; //deprecated;
  sIdentifierSeparator = '.';

type

  { TCodeLexer }

  TCodeLexer = class(TLexer)
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

implementation

uses
  StrUtils;

{ TEscape_Tokenizer }

procedure TEscape_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column); //not need first char, it is not pass from isIdentifier
  //print("Hello "\n"World"); //but add " to the world
  while (IndexInStr(Column, text) and (Lexer.isIdentifier(Text[Column], False))) do
    Inc(Column);
  Lexer.Parser.SetToken(Token(ctlToken, typeEscape, SliceText(Text, Started, Column)));
  Resume := False;
end;

function TEscape_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := Text[Column] = sEscape;
end;

{ TDQString_Tokenizer }

constructor TDQString_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '"';
  CloseSymbol := '"';
end;

{ TSQString_Tokenizer }

constructor TSQString_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '''';
  CloseSymbol := '''';
end;

{ TComment_Tokenizer }

constructor TComment_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '{*';
  CloseSymbol := '*}';
end;

procedure TComment_Tokenizer.SetToken(Text: string);
begin
  Lexer.Parser.SetToken(Token(ctlToken, typeComment, text));
end;

{ TBlockComment_Tokenizer }

procedure TBlockComment_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  while IndexInStr(Column, text) do
  begin
      if (ScanText('*/', Text, Column)) then
      begin
          Resume := False;
          exit;
      end;
      Inc(Column);
  end;
  Inc(Column);;//Eat the second chat //not sure
  Resume := True;
end;

function TBlockComment_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := ScanText('/*', Text, Column);
end;

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

{ TOperator_Tokenizer }

procedure TOperator_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
var
  AOperator: TSardOperator;
begin
  AOperator := Lexer.Operators.Scan(Text, Column);
  if (AOperator <> nil) then
    Column := Column + Length(AOperator.name)
  else
    RaiseError('Unkown operator started with ' + Text[Started]);
  Lexer.Parser.SetOperator(AOperator);
  Resume := false;
end;

function TOperator_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.IsOperator(Text[Column]);
end;

{ TControl_Tokenizer }

procedure TControl_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
var
  AControl: TSardControl;
begin
  AControl := Lexer.Controls.Scan(Text, Column);
  if AControl <> nil then
    Column := Column + Length(AControl.Name)
  else
    RaiseError('Unkown control started with ' + Text[Started]);
  Lexer.Parser.SetControl(AControl);
  Resume := False;
end;

function TControl_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.isControl(Text[Column]);
end;

{ TNumber_Tokenizer }

procedure TNumber_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsNumber(Text[Column], False))) do
    inc(Column);
  Lexer.Parser.SetToken(Token(ctlToken, typeNumber, SliceText(Text, Started, Column)));
  Resume := false;
end;

function TNumber_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.IsNumber(Text[Column], True);
end;

{ TIdentifier_tokenizer }

procedure TIdentifier_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsIdentifier(Text[Column], False))) do
    inc(Column);
  Lexer.Parser.SetToken(Token(ctlToken, typeIdentifier, SliceText(Text, Started, Column)));
  Resume := false;
end;

function TIdentifier_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.IsIdentifier(Text[Column], True);
end;

{ TWhitespace_Tokenizer }

procedure TWhitespace_Tokenizer.Scan(Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsWhiteSpace(Text[Column]))) do
    inc(Column);
  Lexer.Parser.SetWhiteSpaces(SliceText(text, Started, Column));
  Resume := false;
end;

function TWhitespace_Tokenizer.Accept(Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.isWhiteSpace(Text[Column]);
end;

{ TCodeLexer }

constructor TCodeLexer.Create;
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
    //Add('', ctlDeclare);
    //Add('', ctlAssign);

    Add('(', ctlOpenParams);
    Add('[', ctlOpenArray);
    Add('{', ctlOpenBlock);
    Add(')', ctlCloseParams);
    Add(']', ctlCloseArray);
    Add('}', ctlCloseBlock);
    Add(';', ctlEnd);
    Add(',', ctlNext);
    Add(':', ctlDeclare);
    Add(':=', ctlAssign);
  end;

  with Operators do
  begin
    Add(TOpPlus.Create);
    Add(TOpSub.Create);
    Add(TOpMultiply.Create);
    Add(TOpDivide.Create);

    Add(TOpEqual.Create);
    Add(TOpNotEqual.Create);
    Add(TOpAnd.Create);
    Add(TOpOr.Create);
    Add(TOpNot.Create);

    Add(TOpGreater.Create);
    Add(TOpLesser.Create);

    Add(TOpPower.Create);
  end;

  with (Self) do
  begin
      Add(TWhitespace_Tokenizer.Create);
      Add(TBlockComment_Tokenizer.Create);
      Add(TComment_Tokenizer.Create);
      Add(TLineComment_Tokenizer.Create);
      Add(TNumber_Tokenizer.Create);
      Add(TSQString_Tokenizer.Create);
      Add(TDQString_Tokenizer.Create);
      Add(TEscape_Tokenizer.Create);
      Add(TControl_Tokenizer.Create);
      Add(TOperator_Tokenizer.Create); //Register it after comment because comment take /*
      Add(TIdentifier_Tokenizer.Create);//Sould be last one
  end;

end;

function TCodeLexer.IsEOL(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sEOL);
end;

function TCodeLexer.IsWhiteSpace(vChar: char; vOpen: Boolean): Boolean;
begin
  Result := CharInSet(vChar, sWhitespace);
end;

function TCodeLexer.IsControl(vChar: Char): Boolean;
begin
  Result := Controls.IsOpenBy(vChar);
end;

function TCodeLexer.IsOperator(vChar: Char): Boolean;
begin
  Result := Operators.IsOpenBy(vChar);
end;

function TCodeLexer.IsNumber(vChar: Char; vOpen: Boolean): Boolean;
begin
  if (vOpen) then
    Result := CharInSet(vChar, sNumberOpenChars)
  else
    Result := CharInSet(vChar, sNumberChars);
end;

function TCodeLexer.IsSymbol(vChar: Char): Boolean;
begin
  Result := CharInSet(vChar, sSymbolChars) or Symbols.IsOpenBy(vChar);
end;

function TCodeLexer.IsIdentifier(vChar: Char; vOpen: Boolean): Boolean;
begin
  Result := inherited isIdentifier(vChar, vOpen); //we do not need to override it, but it is nice to see it here
end;

end.
