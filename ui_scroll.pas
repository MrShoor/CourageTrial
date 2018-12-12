unit ui_scroll;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avMiniControls, mutils;

const
  cScrollBarWidth = 24;

type
  { TavmDefaultScroll }

  TavmDefaultScroll = class(TavmCustomScrollBar)
  private
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
  public
  end;

implementation

{ TavmDefaultScroll }

procedure TavmDefaultScroll.AfterRegister;
begin
  inherited AfterRegister;
  IsVertical := True;
end;

procedure TavmDefaultScroll.DoValidate;
var barRct: TRectF;
begin
  Canvas.Clear;
  barRct := BarRect();
  barRct := barRct.Expand(-4);
  Canvas.Brush.Color := Vec(0.125, 0.125, 0.125, 1.0);

  Canvas.AddFill(barRct.min, barRct.max);
  Canvas.AddRectangle(barRct.min, barRct.max);

  Canvas.Pen.Color := Vec(0,0,0,1);
  Canvas.Pen.Width := 1;
  Canvas.AddRectangle(Vec(0,0), Size);
end;

end.

