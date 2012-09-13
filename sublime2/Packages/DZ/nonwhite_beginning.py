import sublime
import sublime_plugin


class NonWhiteBeginningCommand(sublime_plugin.TextCommand):

    def run(self, edit, forward=True, sub_words=False):

        if forward:
            classes = sublime.CLASS_WORD_END | sublime.CLASS_PUNCTUATION_END | sublime.CLASS_LINE_START
            if sub_words:
                classes |= sublime.CLASS_SUB_WORD_END
        else:
            classes = sublime.CLASS_WORD_START | sublime.CLASS_PUNCTUATION_START | sublime.CLASS_LINE_END
            if sub_words:
                classes |= sublime.CLASS_SUB_WORD_START

        new_sels = []
        for s in reversed(self.view.sel()):
            if s.empty():
                new_sels.append(self.expand_word(self.view, s.b, classes, forward))
