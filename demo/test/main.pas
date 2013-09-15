unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls,
  sard;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

end.

