unit untGraphics;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, bWorld, avCanvas, mutils;

type
  { TbFlyOutMessage }

  TbFlyOutMessage = class(TbGraphicalObject)
  private
    FStartPos  : TVec3;
    FStartPhase: Single;
    FStartTime : Integer;
    FLiveTime  : Integer;
  public
    procedure SetState(const APos: TVec3; const AMsg: String; const AColor: TVec4; const ALiveTime: Integer = 1700);

    procedure Draw(); override;
    procedure AfterConstruction; override;
  end;

implementation

{ TbFlyOutMessage }

procedure TbFlyOutMessage.SetState(const APos: TVec3; const AMsg: String;
  const AColor: TVec4; const ALiveTime: Integer);
var tb: ITextBuilder;
    tl: ITextLines;
begin
  FStartPos := APos;
  FStartTime := World.GameTime;
  FLiveTime := World.GameTime + ALiveTime;

  Canvas.Clear;
  Canvas.Font.Color := AColor;
  tb := Canvas.TextBuilder;
  tb.Align := TLineAlign.laCenter;
  tb.WriteLn(AMsg);
  tl := tb.Finish();
  tl.VAlign := 0.5;
  tl.BoundsX := Vec(0,0);
  tl.BoundsY := Vec(0,0);
  Canvas.AddText(tl);
end;

procedure TbFlyOutMessage.Draw;
var dt: Integer;
    fi, r, y: Single;
begin
  if FLiveTime < World.GameTime then
    SafeDestroy;

  dt := World.GameTime - FStartTime;
  fi := dt * 0.001 + FStartPhase;
  r := dt * 0.0005;
  y := dt * 0.001;
  Pos := FStartPos + Vec(r * Cos(fi), y, r * Sin(fi));
  inherited Draw;
end;

procedure TbFlyOutMessage.AfterConstruction;
begin
  inherited AfterConstruction;
  Canvas.Font.Size := 32;
  Canvas.Font.Style := [TGlyphStyle.gsBold];
  FStartPhase := Random()*Pi*2;
end;

end.

