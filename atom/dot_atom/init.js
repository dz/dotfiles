"use babel";

atom.commands.add('atom-workspace', 'dz:close-other-panes', function() {
  let panes = atom.workspace.getPaneItems();
  let activePane = atom.workspace.getActivePaneItem();
  for (let pane of panes) {
    if (pane !== activePane) {
        pane.destroy();
    }
  }
});

atom.commands.add('atom-workspace', 'dz:indent-selection', function() {
  let editor = atom.workspace.getActivePaneItem();
  let cursor = editor.getLastCursor();
  let savedPosition = cursor.getScreenPosition();
  // if no text is selected, we select the lines the cursors are on
  if (editor.getSelectedText().length == 0) {
    editor.selectLinesContainingCursors();
  }
  editor.autoIndentSelectedRows();
  // put the cursor where it was
  cursor = editor.getLastCursor();
  cursor.setScreenPosition(savedPosition);
});
