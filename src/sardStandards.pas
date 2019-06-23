unit sardStandards;
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
  mnUtils,
  mnClasses, sardClasses, sardParsers;

type
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


implementation

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
  collect(SliceText(text, Started, Column));
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
  Result := Lexer.IsControl(Text[Column]);
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

end.
