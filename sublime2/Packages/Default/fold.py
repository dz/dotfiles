import sublime, sublime_plugin

class FoldUnfoldCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        new_sel = []
        for s in self.view.sel():
            r = s
            empty_region = r.empty()
            if empty_region:
                r = sublime.Region(r.a - 1, r.a + 1)

            unfolded = self.view.unfold(r)
            if len(unfolded) == 0:
                self.view.fold(s)
            elif empty_region:
                for r in unfolded:
                    new_sel.append(r)

        if len(new_sel) > 0:
            self.view.sel().clear()
            for r in new_sel:
                self.view.sel().add(r)

class FoldCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        new_sel = []
        for s in self.view.sel():
            if s.empty():
                r = self.view.indentation(s.a)
                if not r.empty():
                    r = sublime.Region(r.a - 1, r.b - 1)
                    self.view.fold(r)
                    new_sel.append(r)
                else:
                    new_sel.append(s)
            else:
                if self.view.fold(s):
                    new_sel.append(s)
                else:
                    r = self.view.indentation(s.a)
                    if not r.empty():
                        r = sublime.Region(r.a - 1, r.b - 1)
                        self.view.fold(r)
                        new_sel.append(r)
                    else:
                        new_sel.append(s)

        self.view.sel().clear()
        for r in new_sel:
            self.view.sel().add(r)


class UnfoldCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        new_sel = []
        for s in self.view.sel():
            unfold = s
            if s.empty():
                unfold = sublime.Region(s.a - 1, s.a + 1)

            unfolded = self.view.unfold(unfold)
            if len(unfolded) == 0 and s.empty():
                unfolded = self.view.unfold(self.view.full_line(s.b))

            if len(unfolded) == 0:
                new_sel.append(s)
            else:
                for r in unfolded:
                    new_sel.append(r)

        if len(new_sel) > 0:
            self.view.sel().clear()
            for r in new_sel:
                self.view.sel().add(r)


class UnfoldAllCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        self.view.unfold(sublime.Region(0, self.view.size()))
