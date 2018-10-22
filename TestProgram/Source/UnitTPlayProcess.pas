unit UnitTPlayProcess;

interface

uses
  Classes;

type
  TPlayProcess = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TPlayProcess.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TPlayProcess }

procedure TPlayProcess.Execute;
begin
  { Place thread code here }
end;

end.
