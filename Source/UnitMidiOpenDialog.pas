//---------------------------------------------------------------------------
//
//  Program:     UnitOpenDialog.pas in MidiAndMusicXmlPlayer
//
//  Project:     MidiAndMusicXmlPlayer.dpr / MidiAndMusicXmlPlayer.lpi
//
//  Purpose:     Not used own Open-Dialog
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: Use the functions for TreeView to build the file-tree
//---------------------------------------------------------------------------

unit UnitMidiOpenDialog;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$ifdef Windows}
  Windows,
  Messages,
{$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, FileCtrl,
  UnitMidiDefinitions, UnitMessage;
// Some space around boxes and buttons
const GLUE = 5;

const
  SAnyMask = {$ifdef Darwin}'*'{$else}'*.*'{$endif};
  PathDelim = {$ifdef Darwin}'/'{$else}'\'{$endif};
  faSymLink = $00000040;

const
  // Select the icons used from the Icon bitmap
  // Note: 0 = yellow folder
  //       2 = yellow Win8 like
  IconFolder = 3;  // Blue folder
  IconFile = 1;    // The Midi/MusicXml icon

type

  { TFormMidiOpenDialog }

  TFormMidiOpenDialog = class(TForm)
    TreeView: TTreeView;
    ButtonOk: TButton;
    EditFileName: TEdit;
    ImageFolders: TImage;
    Timer: TTimer;
    procedure OnActivate(Sender: TObject);
{$ifdef FPC}
    procedure OnDropFiles(Sender: TObject; const FileNames: array of String);
{$endif}
    procedure OnResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure OnDblClick(Sender: TObject);
    procedure OnEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure ButtonOkClick(Sender: TObject);
    procedure OnClose(Sender: TObject; var Action: TCloseAction);
{$HINTS OFF}
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
{$HINTS ON}
 
  public
    { Public declarations }
    SelectedFileName: string;
    SetVisible: boolean;
    procedure FindFiles(Dir: string; RootNode: TTreeNode);
    function DirNotEmpty(Dir: string; RootNode: TTreeNode): boolean;
    function AddNode(Node: TTreeNode; Name: string): TTreeNode;
    function AddSubNode(Node: TTreeNode; Name: string;
                        IconNr: integer; Description: string): TTreeNode;
    function ShortPath(Name: string): string;
    procedure SavePosition;
  end;

var
  FormMidiOpenDialog: TFormMidiOpenDialog;

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.DFM}
{$endif}

//---------------------------------------------------------------------------
//
//     Function:   AddNode
//
//     Purpose:    To add a node to the tree view. Show a folder icon.
//
//     Parameters: Node = the tree view node
//                 Name = text shown (music or download)
//
//     Returns:    void
//
//     Notes:      <none>
//
//---------------------------------------------------------------------------

function TFormMidiOpenDialog.AddNode(Node: TTreeNode; Name: string): TTreeNode;
var
  NewNode: TTreeNode;

  begin
  NewNode:=TreeView.Items.Add(Node,Name);
  NewNode.ImageIndex:=IconFolder;
  NewNode.SelectedIndex:=IconFolder;
  NewNode.Data:=nil;
  AddNode:=NewNode;
  end;

//---------------------------------------------------------------------------
//
//     Function:   AddSubNode
//
//     Purpose:    To add a subnode to the tree view. Show a folder or
//                 file icon.
//
//     Parameters: Node = the tree view node
//                 Name = text shown (music or download)
//                 IconNr = The icon according to the icon file.
//                 (May be different on PC and MAC. MAC is p.t. blue)
//                 See definitions IconFolder and IconFile
//
//     Returns:    void
//
//     Notes:      <none>
//
//---------------------------------------------------------------------------
function TFormMidiOpenDialog.AddSubNode(Node: TTreeNode; Name: string;
  IconNr: integer; Description: string): TTreeNode;

var
  RootNode: TTreeNode;
  PDescription: ^string;

  begin
  RootNode:=Node;
  Node:=TreeView.Items.AddChild(RootNode,Name);
  Node.ImageIndex:=IconNr;
  Node.SelectedIndex:=IconNr;
  New(PDescription);                 //// OBS: Add freeing the memory !!!!
  PDescription^:=Description;
  Node.Data:=PDescription;
  RootNode.Expand(true);
  AddSubNode:=Node;
  end;

//---------------------------------------------------------------------------
//
//     Function:   SavePosition
//
//     Purpose:    The save the position and size of the window
//                 in order to show it at same place next time
//
//     Parameters: <none>
//
//     Returns:    void
//
//     Notes:      <none>
//
//---------------------------------------------------------------------------

procedure TFormMidiOpenDialog.SavePosition;
{$ifdef SaveAllOptions}
  var
    WindowPosition: TextFile;
{$endif}

    begin
{$ifdef SaveAllOptions}
    Home:=GetEnvironmentVariable('HOME');
    if not DirectoryExists(Home+'/MidiOptions') then
      MkDir(Home+'/MidiOptions');
    AssignFile(WindowPosition,Home+'/MidiOptions/Default.txt');
    rewrite(WindowPosition);
    writeln(WindowPosition,Left);
    writeln(WindowPosition,Top);
    writeln(WindowPosition,Width);
    writeln(WindowPosition,Height);
    CloseFile(WindowPosition);
{$endif}
    end;

//---------------------------------------------------------------------------
//
//     Function:   FindFiles
//
//     Purpose:    To traverse the whole file structure and add the
//                 elements to the tree view.
//
//     Parameters: Dir = current directory
//                 RootNode = current node in the tree view
//
//     Returns:    void
//
//     Notes:      <none>
//
//---------------------------------------------------------------------------

procedure TFormMidiOpenDialog.FindFiles(Dir: string; RootNode: TTreeNode);

var
  Info: TSearchRec;
  Node: TTreeNode;
  MusicXmlFile: textfile;
  MidiFile: textfile;
  s1: string;
  s2: string;

  c1: char;
  c2: char;
  c3: char;
  c4: char;

  begin
  // Find sub-directories and recurse
  Node:=nil;
  if FindFirst(Dir+SAnyMask,faDirectory,Info)=0 then
    try
      repeat
      if (Info.Attr and faDirectory<>0) and
         (Info.Name<>'.') and (Info.Name<>'..') and
         (Info.Attr and faSymLink=0) then
        begin // Dir - add name to directory path
        if DirNotEmpty(Dir+Info.Name+PathDelim,Node) then
        Node:=AddSubNode(RootNode,Info.Name,IconFolder,Dir+Info.Name);
        FindFiles(Dir+Info.Name+PathDelim,Node);
        end
      else
        begin // File - only Midi and MusicXml.
        if ((Info.Name<>'.') and (Info.Name<>'..')) and
           (LowerCase(ExtractFileExt(Info.Name))='.mid') or
           (LowerCase(ExtractFileExt(Info.Name))='.xml') then
          begin
          if LowerCase(ExtractFileExt(Info.Name))='.xml' then
            begin
            {$i-}
            AssignFile(MusicXmlFile,Dir+Info.Name);
            if IORESULT=0 then
            reset(MusicXmlFile);
            {$i+}
            if IORESULT=0 then
              begin
              // A MusicXml file has the word MusicXML in second line
              if not eof(MusicXmlFile) then readln(MusicXmlFile,s1);
              if not eof(MusicXmlFile) then readln(MusicXmlFile,s2);
              if (Pos('MusicXML',s1)>0) or (Pos('MusicXML',s2)>0) then
              Node:=AddSubNode(RootNode, Info.Name,IconFile,Dir+Info.Name);
              CloseFile(MusicXmlFile);
              end;
            end
          else
            begin // Midi File - Extension = ".mid"
            if FileExists(Dir+Info.Name) then
              begin
              {$i-}
              AssignFile(MidiFile,Dir+Info.Name);
              if IORESULT=0 then
              reset(MidiFile);
              {$i+}
              if IORESULT=0 then
                begin
                // A midi file has the characters "MThd" in the start
                c1:=chr(0);
                c2:=chr(0);
                c3:=chr(0);
                c4:=chr(0);
                if not eof(MidiFile) then read(MidiFile,c1);
                if not eof(MidiFile) then read(MidiFile,c2);
                if not eof(MidiFile) then read(MidiFile,c3);
                if not eof(MidiFile) then read(MidiFile,c4);
                if (c1='M') and (c2='T') and (c3='h') and (c4='d') then
                Node:=AddSubNode(RootNode,Info.Name,IconFile,Dir+Info.Name);
                CloseFile(MidiFile);
                end;
              end;
            end;
          end;
        end;
      until FindNext(Info)<>0;
    finally
    FindClose(Info);
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:   DirNotEmpty
//
//     Purpose:    To traverse the whole file structure and test
//                 if directory is empty (i.e. contains no Midi nor MusicXml)
//
//     Parameters: Dir = current directory
//                 RootNode = current node in the tree view
//
//     Returns:    void
//
//     Notes:      <none>
//
//---------------------------------------------------------------------------

function TFormMidiOpenDialog.DirNotEmpty(Dir: string; RootNode: TTreeNode): boolean;

var
  Info: TSearchRec;
  Node: TTreeNode;
  NotEmpty: boolean;
  MusicXmlFile: textfile;
  s1: string;
  s2: string;

  begin
  // Find sub-directories and call recurse
  Node:=nil;
  NotEmpty:=false;
  if FindFirst(Dir+SAnyMask,faDirectory,Info)=0 then
    try
      NotEmpty:=false;
      repeat
      if (Info.Attr and faDirectory<>0) and
         (Info.Name<>'.') and (Info.Name<>'..') and
         (Info.Attr and faSymLink=0) then
        begin // Dir
        NotEmpty:=NotEmpty or DirNotEmpty(Dir+Info.Name+PathDelim,Node);
        end
      else
        begin // File
        if ((Info.Name<>'.') and (Info.Name<>'..')) and
           (LowerCase(ExtractFileExt(Info.Name))='.mid') or
           (LowerCase(ExtractFileExt(Info.Name))='.xml') then
          begin
          if LowerCase(ExtractFileExt(Info.Name))='.xml' then
            begin
            {$i-}
            AssignFile(MusicXmlFile,PChar(Dir+Info.Name));
            if IORESULT=0 then
            reset(MusicXmlFile);
            {$i+}
            if IORESULT=0 then
              begin
              if not eof(MusicXmlFile) then readln(MusicXmlFile,s1);
              if not eof(MusicXmlFile) then readln(MusicXmlFile,s2);
              if Pos('MusicXML',s1)>0 then NotEmpty:=true;
              if Pos('MusicXML',s2)>0 then NotEmpty:=true;
              CloseFile(MusicXmlFile);
              end;
            end
          else
            begin
            if FileExists(Dir+Info.Name) then NotEmpty:=true;
            end;
          end;
        end;
      until FindNext(Info)<>0;
    finally
    FindClose(Info);
    end;
    DirNotEmpty:=NotEmpty;
  end;



procedure TFormMidiOpenDialog.TimerTimer(Sender: TObject);
var
  Node: TTreeNode;
  PDescription: ^string;

  begin
  Node:=TreeView.Selected;
  if Node<>nil then
    begin
    PDescription:=Node.Data;
    if (PDescription<>nil) then
    if (LowerCase(ExtractFileExt(PDescription^))='.mid') or
       (LowerCase(ExtractFileExt(PDescription^))='.xml') then
      begin
      SelectedFileName:=PDescription^;
      EditFileName.Text:=ShortPath(PDescription^);
      end;
    end;
  end;

procedure TFormMidiOpenDialog.OnActivate(Sender: TObject);
var
  ImageList: TImageList;
  Home: string;
  WindowPosition: TextFile;
  L,T,W,H: integer;   // Left, Top, Width, Height

  NodeMusicTop: TTreeNode;
  NodeDownloadTop: TTreeNode;
  NodeDesktopTop: TTreeNode;
{$ifndef FPC}
  HomeC: CString;
{$endif}

  begin
  L:=Left;
  T:=Top;
  W:=Width;
  H:=Height;
  SelectedFileName:='';
  EditFileName.Text:='';
{$ifdef FPC}
  Home:=GetEnvironmentVariable('HOME');
{$else}
  GetEnvironmentVariable('HOMEPATH',@HomeC,255);
  Home:=PascalString(HomeC);
{$endif}
  if FileExists(Home+PathDelim+'MidiOptions'+PathDelim+'Default.txt') then
    begin
    AssignFile(WindowPosition,Home+PathDelim+'MidiOptions'+PathDelim+'Default.txt');
    reset(WindowPosition);
    if not eof(WindowPosition) then readln(WindowPosition,L);
    if not eof(WindowPosition) then readln(WindowPosition,T);
    if not eof(WindowPosition) then readln(WindowPosition,W);
    if not eof(WindowPosition) then readln(WindowPosition,H);
    CloseFile(WindowPosition);
    end;
  Left:=L;
  Top:=T;
  Width:=W;
  Height:=H;
  ImageList:=TImageList.Create(nil);
  ImageList.Clear;
  ImageList.Add(ImageFolders.Picture.Bitmap,ImageFolders.Picture.Bitmap);
  TreeView.Images:=ImageList;

  NodeMusicTop:=AddNode(TreeView.TopItem,'Music');
  NodeDownloadTop:=AddNode(TreeView.TopItem,'Download');
  NodeDesktopTop:=AddNode(TreeView.TopItem,'Desktop');
  FindFiles(Home+PathDelim+'Music'+PathDelim,NodeMusicTop);
  FindFiles(Home+PathDelim+'Downloads'+PathDelim,NodeDownloadTop);
  FindFiles(Home+PathDelim+'Desktop'+PathDelim,NodeDesktopTop);

  SelectedFileName:='';
  TreeView.FullExpand;
  end;

{$ifdef FPC}
procedure TFormMidiOpenDialog.OnDropFiles(Sender: TObject;
  const FileNames: array of String);

var s: string;

  begin
  s:=FileNames[0];
  FileName:=s;
  FormMessage.CreateMessageWindow;
  SystemState:=MidiStarting;
  if (High(FileNames)>0) and (not FileExists(FileName) and
     (not FileExists(FileName))) then
    begin
    SystemState:=MidiNoFile;
    Visible:=false;
    end;
  end;
{$endif}

function TFormMidiOpenDialog.ShortPath(Name: string): string;

var
  Home: string;
{$ifndef FPC}
  HomeC: CString;
{$endif}
  p: integer;

  begin
{$ifdef FPC}
  Home:=GetEnvironmentVariable('HOME');
{$else}
  GetEnvironmentVariable('HOMEPATH',@HomeC,255);
  Home:=PascalString(HomeC);
{$endif}
  p:=Pos(Home,Name);
  // Pass the HOME and '/' or '\'
  if p=1 then ShortPath:=copy(Name,Length(Home)+2,Length(Name)-Length(Home));
    end;

procedure TFormMidiOpenDialog.OnDblClick(Sender: TObject);

var
  Node: TTreeNode;
  PDescription: ^string;

  begin
  Node:=TreeView.Selected;
  if Node<>nil then
    begin
    PDescription:=Node.Data;
    if PDescription<>nil then
      begin
      SelectedFileName:=PDescription^;
      EditFileName.Text:=ShortPath(PDescription^);
      end;
    ButtonOkClick(Sender);
    end;
  end;

procedure TFormMidiOpenDialog.OnEdited(Sender: TObject; Node: TTreeNode;
  var S: String);

var
  NewFileName: string;
  OldFilePath: string;
  NewFilePath: string;
  PDescription: ^string;
  p: integer; // Position of delimiter '/'
  i: integer; // Loop variable  for positions

  begin
  PDescription:=Node.Data;
  OldFilePath:=PDescription^;
  // Find the path part
  p:=0;
  for i:=1 to Length(OldFilePath) do if OldFilePath[i]=PathDelim then p:=i;
  NewFilePath:=copy(OldFilePath,1,p);
  NewFileName:=S;
  NewFilePath:=NewFilePath+NewFileName;
  RenameFile(OldFilePath,NewFilePath);
  PDescription^:=NewFilePath;
  SelectedFileName:=PDescription^;
  EditFileName.Text:=ShortPath(PDescription^);
  ButtonOk.SetFocus;
  end;


//---------------------------------------------------------------------------
//
//     Function:   OnResize
//
//     Purpose:    To handle the standard windows event OnResize
//                 This window can be of any size and the elements
//                 are resized accordingly
//
//     Parameters: Sender = Windows standard
//
//     Returns:    void
//
//     Notes:      <none>
//
//---------------------------------------------------------------------------

procedure TFormMidiOpenDialog.OnResize(Sender: TObject);

  begin
  ButtonOk.Top:=ClientHeight-EditFileName.Height-3*GLUE;
  ButtonOk.Width:=ClientWidth div 10;
  ButtonOk.Height:=EditFileName.Height;
  ButtonOk.Left:=ClientWidth-ButtonOk.Width-6*GLUE;
  EditFileName.Top:=ClientHeight-EditFileName.Height-3*GLUE;
  EditFileName.Left:=GLUE;
  EditFileName.Width:=ClientWidth-ButtonOk.Width-8*GLUE;
  TreeView.Height:=ClientHeight-ButtonOk.Height-4*GLUE;
  end;


procedure TFormMidiOpenDialog.ButtonOkClick(Sender: TObject);

  begin
  Visible:=false;
  if FileExists(SelectedFileName) then
    begin
    FileName:=SelectedFileName;
    FormMessage.CreateMessageWindow;
    SystemState:=MidiStarting;
    end
  else
    begin
    SystemState:=MidiNoFile;
    end;
  end;

procedure TFormMidiOpenDialog.OnClose(Sender: TObject; var Action: TCloseAction);

var
  WindowPosition: TextFile;
  Home: string;
{$ifndef FPC}
  HomeC: CString;
{$endif}

  begin
{$ifdef FPC}
  Home:=GetEnvironmentVariable('HOME');
{$else}
  GetEnvironmentVariable('HOMEPATH',@HomeC,255);
  Home:=PascalString(HomeC);
{$endif}
  if not DirectoryExists(Home+PathDelim+'MidiOptions') then
    MkDir(Home+PathDelim+'MidiOptions');
  AssignFile(WindowPosition,Home+PathDelim+'MidiOptions'+PathDelim+'Default.txt');
  rewrite(WindowPosition);
  writeln(WindowPosition,Left);
  writeln(WindowPosition,Top);
  writeln(WindowPosition,Width);
  writeln(WindowPosition,Height);
  CloseFile(WindowPosition);
  end;


procedure TFormMidiOpenDialog.OnMouseDown(Sender: TObject;
  Button: TMouseButton;
{$WARNINGS ON}
  Shift: TShiftState; X, Y: Integer);
{$WARNINGS OFF}
////var DragObject: TDragObject;

  begin
{$ifdef TestDragDrop}
  FileListBoxDrag.FileName:='Folder.bmp';
////  with Sender as TFileListBox do
    begin
    FileListBoxDrag.DragMode:=dmManual;
    FileListBoxDrag.DragCursor:=crDrag;
////    FileListBoxDrag.BeginDrag(false);
////    DragObject.InheritsFrom(FileListBoxDrag);
    DragObject:=TDragObject.Create;
////    DragObject.Assign(FileListBoxDrag);
////    FileListBoxDrag.Assign(DragObject);
    DragObject:=@FileListBoxDrag;
    DoStartDrag(DragObject);
    ////DoDragDrop();
    Screen.Cursor:=crDrag;
    end;
{$endif}
  end;

end.

{This example will show you how your application
will be able to copy files from your application to
Windows Explorer using Drag'n Drop.
Exactly the way it is done by the OS itself!

Create a new application containing just one unit,
called 'Unit1'. Drop a FileListBox and a DirectoryListBox on to the form,
leave their names the way they are.
Connect FileListBox1 with DirectoryListBox1 by setting the FileList-property of
DirectoryListBox1. Make sure that the MultiSelect-property of FileListBox1 is set to 'True'!

The best thing you can do now is to replace all text with the code below:}

//---------------------------------------------

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls, FileCtrl, ActiveX, ShlObj, ComObj;

type
  TForm1 = class(TForm, IDropSource)
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    procedure FileListBox1MouseDown(Sender: TObject; Button:
      TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FileListBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X,
      Y: Integer);
  private
    FDragStartPos: TPoint;
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function GetFileListDataObject(const Directory: string; Files:
  TStrings):
  IDataObject;
type
  PArrayOfPItemIDList = ^TArrayOfPItemIDList;
  TArrayOfPItemIDList = array[0..0] of PItemIDList;
var
  Malloc: IMalloc;
  Root: IShellFolder;
  FolderPidl: PItemIDList;
  Folder: IShellFolder;
  p: PArrayOfPItemIDList;
  chEaten: ULONG;
  dwAttributes: ULONG;
  FileCount: Integer;
  i: Integer;
begin
  Result := nil;
  if Files.Count = 0 then
    Exit;
  OleCheck(SHGetMalloc(Malloc));
  OleCheck(SHGetDesktopFolder(Root));
  OleCheck(Root.ParseDisplayName(0, nil,
    PWideChar(WideString(Directory)),
    chEaten, FolderPidl, dwAttributes));
  try
    OleCheck(Root.BindToObject(FolderPidl, nil, IShellFolder,
      Pointer(Folder)));
    FileCount := Files.Count;
    p := AllocMem(SizeOf(PItemIDList) * FileCount);
    try
      for i := 0 to FileCount - 1 do
      begin
        OleCheck(Folder.ParseDisplayName(0, nil,
          PWideChar(WideString(Files[i])), chEaten, p^[i],
          dwAttributes));
      end;
      OleCheck(Folder.GetUIObjectOf(0, FileCount, p^[0], IDataObject,
        nil,
        Pointer(Result)));
    finally
      for i := 0 to FileCount - 1 do begin
        if p^[i] <> nil then Malloc.Free(p^[i]);
      end;
      FreeMem(p);
    end;
  finally
    Malloc.Free(FolderPidl);
  end;
end;

function TForm1.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Longint): HResult; stdcall;
begin
  if fEscapePressed or (grfKeyState and MK_RBUTTON = MK_RBUTTON) then
  begin
    Result := DRAGDROP_S_CANCEL
  end else if grfKeyState and MK_LBUTTON = 0 then
  begin
    Result := DRAGDROP_S_DROP
  end else
  begin
    Result := S_OK;
  end;
end;

function TForm1.GiveFeedback(dwEffect: Longint): HResult; stdcall;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

procedure TForm1.FileListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragStartPos.x := X;
    FDragStartPos.y := Y;
  end;
end;

procedure TForm1.FileListBox1MouseMove(Sender: TObject; Shift:
  TShiftState;
  X, Y: Integer);
const
  Threshold = 5;
var
  SelFileList: TStrings;
  i: Integer;
  DataObject: IDataObject;
  Effect: DWORD;
begin
  with Sender as TFileListBox do
  begin
    if (SelCount > 0) and (csLButtonDown in ControlState)
      and ((Abs(X - FDragStartPos.x) >= Threshold)
      or (Abs(Y - FDragStartPos.y) >= Threshold)) then
      begin
      Perform(WM_LBUTTONUP, 0, MakeLong(X, Y));
      SelFileList := TStringList.Create;
      try
        SelFileList.Capacity := SelCount;
        for i := 0 to Items.Count - 1 do
          if Selected[i] then SelFileList.Add(Items[i]);
        DataObject := GetFileListDataObject(Directory, SelFileList);
      finally
        SelFileList.Free;
      end;
      Effect := DROPEFFECT_NONE;
      DoDragDrop(DataObject, Self, DROPEFFECT_COPY, Effect);
    end;
  end;
end;

{$ifdef SomeCodeProposedBySomeOne}
{$ifndef PFC}
initialization
  OleInitialize(nil);
finalization
  OleUninitialize;
{$endif}
{$endif}
end.

