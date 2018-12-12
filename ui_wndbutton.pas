unit ui_wndbutton;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avCanvas, avMiniControls, mutils;

type
  TavmWndCheckButton = class(TavmCustomCheckbox)
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
  public
  end;

  { TavmOtherInventoryBtn }

  TavmOtherInventoryBtn = class(TavmCustomButton)
  protected
    procedure AfterRegister; override;
    procedure DoValidate; override;
  end;

implementation

{ TavmOtherInventoryBtn }

procedure TavmOtherInventoryBtn.AfterRegister;
begin
  inherited AfterRegister;
  Size := Vec(190, 74);
end;

procedure TavmOtherInventoryBtn.DoValidate;
var tl: ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;
  if Downed then
    Canvas.AddSprite(Vec(0,0), Size, 'ui\buttons\button2.png')
  else
    Canvas.AddSprite(Vec(0,0), Size, 'ui\buttons\button1.png');

  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Size := 26;
  with Canvas.TextBuilder do
  begin
    Align := laCenter;
    Write(Text);
    tl := Finish();
    tl.BoundsX := Vec(0, Size.x);
    tl.BoundsY := Vec(0, Size.y);
    tl.VAlign := 0.5;
    Canvas.AddText(tl);
  end;
end;

{ TavmWndCheckButton }

procedure TavmWndCheckButton.AfterRegister;
begin
  inherited AfterRegister;
  Size := Vec(190, 74);
end;

procedure TavmWndCheckButton.DoValidate;
var tl: ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;
  if Checked then
    Canvas.AddSprite(Vec(0,0), Size, 'ui\buttons\button2.png')
  else
    Canvas.AddSprite(Vec(0,0), Size, 'ui\buttons\button1.png');

  Canvas.Font.Color := Vec(1,1,1,1);
  Canvas.Font.Size := 26;
  with Canvas.TextBuilder do
  begin
    Align := laCenter;
    Write(Text);
    tl := Finish();
    tl.BoundsX := Vec(0, Size.x);
    tl.BoundsY := Vec(0, Size.y);
    tl.VAlign := 0.5;
    Canvas.AddText(tl);
  end;
end;

end.

