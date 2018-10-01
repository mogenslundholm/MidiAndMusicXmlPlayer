unit UnitMixer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormMixer = class(TForm)
    ScrollBarMain: TScrollBar;
    Edit1: TEdit;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    ScrollBar5: TScrollBar;
    ScrollBar6: TScrollBar;
    ScrollBar7: TScrollBar;
    ScrollBar8: TScrollBar;
    ScrollBar9: TScrollBar;
    ScrollBar10: TScrollBar;
    ScrollBar11: TScrollBar;
    ScrollBar12: TScrollBar;
    ScrollBar13: TScrollBar;
    ScrollBar14: TScrollBar;
    ScrollBar15: TScrollBar;
    ScrollBar16: TScrollBar;
    procedure OnCreate(Sender: TObject);
  private
    { Private declarations }
    ScrollBars: array[1..16] of TScrollBar;

  public
    { Public declarations }
  end;

var
  FormMixer: TFormMixer;

implementation

{$R *.DFM}

procedure TFormMixer.OnCreate(Sender: TObject);

var i: integer;

  begin
  ScrollBars[1]:=ScrollBar1;
  end;

end.
