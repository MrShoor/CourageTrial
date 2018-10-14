unit untGraphics;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, bGraphicalObjects, avCanvas, mutils;

type
  { TbFlyOutMessage }

  TbFlyOutMessage = class(TbGraphicalObject)
  private
    FStartPos : TVec3;
    FStartTime: Integer;
    FLiveTime : Integer;
  public
    procedure SetState(const APos: TVec3; const AMsg: String; const ALiveTime: Integer);

    procedure Draw(); override;
  end;

implementation

{ TbFlyOutMessage }

procedure TbFlyOutMessage.SetState(const APos: TVec3; const AMsg: String; const ALiveTime: Integer);
var tb: ITextBuilder;
    tl: ITextLines;
begin
  FStartPos := APos;
  FStartTime := World.GameTime;
  FLiveTime := World.GameTime + ALiveTime;

  Canvas.Clear;
  tb := Canvas.TextBuilder;
  tb.Align := TLineAlign.laCenter;
  tb.WriteLn(AMsg);
  tl := tb.Finish();
  tl.VAlign := 0.5;
  Canvas.AddText(tl);
end;

procedure TbFlyOutMessage.Draw;
var dt: Integer;
    fi, r, y: Single;
begin
  if FLiveTime < World.GameTime then
  begin
    Exit; //todo fix leak
  end;

  dt := World.GameTime - FStartTime;
  fi := dt * 0.004;
  r := dt * 0.001;
  y := dt * 0.005;
  Pos := FStartPos + Vec(r * Cos(fi), y, r * Sin(fi));
  inherited Draw;
end;

end.

