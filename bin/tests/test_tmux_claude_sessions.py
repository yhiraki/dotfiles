"""tmux-claude-sessions の突合・コマンド生成ロジックのテスト。"""

import importlib.util
import pathlib
import unittest

_SCRIPT = pathlib.Path(__file__).resolve().parent.parent / "tmux-claude-sessions"
_spec = importlib.util.spec_from_loader(
    "tmux_claude_sessions",
    importlib.machinery.SourceFileLoader("tmux_claude_sessions", str(_SCRIPT)),
)
tcs = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(tcs)


class TestParseTmuxRef(unittest.TestCase):
    def test_parses_session_window_pane(self):
        self.assertEqual(
            tcs.parse_tmux_ref("default:@22.%28"), ("default", "@22", "%28")
        )

    def test_session_name_may_contain_colon(self):
        self.assertEqual(
            tcs.parse_tmux_ref("my:sess:@1.%2"), ("my:sess", "@1", "%2")
        )

    def test_returns_none_for_garbage(self):
        self.assertIsNone(tcs.parse_tmux_ref(""))
        self.assertIsNone(tcs.parse_tmux_ref("default"))
        self.assertIsNone(tcs.parse_tmux_ref(None))


class TestSanitizeClaudeArgs(unittest.TestCase):
    def test_drops_resume_flags(self):
        self.assertEqual(
            tcs.sanitize_claude_args(["--chrome", "--resume", "abc", "-c"]),
            ["--chrome"],
        )

    def test_drops_print_and_its_prompt(self):
        self.assertEqual(
            tcs.sanitize_claude_args(["-p", "/calendar-capture-auto", "--chrome"]),
            ["--chrome"],
        )

    def test_keeps_unknown_flags(self):
        self.assertEqual(
            tcs.sanitize_claude_args(["--chrome", "--verbose"]),
            ["--chrome", "--verbose"],
        )


class TestProcStartMatches(unittest.TestCase):
    def test_matches_same_wall_clock(self):
        self.assertTrue(
            tcs.proc_start_matches("Tue Aug 18 10:22:25 2026",
                                   "Tue Aug 18 10:22:25 2026")
        )

    def test_matches_across_utc_and_local_notation(self):
        # ~/.claude/sessions は procStart を UTC、ps の lstart はローカル時刻で出す
        self.assertTrue(
            tcs.proc_start_matches("Tue Aug 18 01:22:25 2026",   # UTC
                                   "Tue Aug 18 10:22:25 2026")   # JST
        )

    def test_rejects_different_process(self):
        self.assertFalse(
            tcs.proc_start_matches("Sun Aug 16 01:00:00 2026",
                                   "Tue Aug 18 10:22:25 2026")
        )

    def test_handles_single_digit_day_padding(self):
        self.assertTrue(
            tcs.proc_start_matches("Tue Aug  4 01:00:00 2026",
                                   "Tue Aug  4 10:00:00 2026")
        )

    def test_unparsable_input_is_not_a_match(self):
        self.assertFalse(tcs.proc_start_matches("garbage", "Tue Aug 18 10:22:25 2026"))
        self.assertFalse(tcs.proc_start_matches("", ""))


class TestBuildResumeCommand(unittest.TestCase):
    def test_includes_session_id_and_kept_args(self):
        self.assertEqual(
            tcs.build_resume_command(
                {"sessionId": "0bfdb96d-929f-4edd-89b2-449339ee858c",
                 "args": ["--chrome"]}
            ),
            "claude --chrome --resume 0bfdb96d-929f-4edd-89b2-449339ee858c",
        )

    def test_works_without_args(self):
        self.assertEqual(
            tcs.build_resume_command({"sessionId": "sid", "args": []}),
            "claude --resume sid",
        )


class TestSelectLiveClaudeSessions(unittest.TestCase):
    def setUp(self):
        self.ps = {
            391: {"lstart": "Mon Aug 17 22:52:32 2026", "command": "claude --chrome"},
            1015: {"lstart": "Tue Aug 18 10:22:25 2026", "command": "claude --chrome"},
        }
        # ペイン ID だけだと別の tmux サーバの同番ペインに誤って当たる
        self.live_panes = {"default:@15.%19", "default:@22.%28"}

    def _rec(self, **kw):
        base = {
            "pid": 391,
            "sessionId": "cbf3c6a9-a5b7-45fc-a727-c20db56cf5d7",
            "procStart": "Mon Aug 17 22:52:32 2026",
            "startedAt": 100,
            "kind": "interactive",
            "tmux": "default:@15.%19",
            "cwd": "/repo",
        }
        base.update(kw)
        return base

    def test_picks_matching_live_session(self):
        got = tcs.select_live_claude_sessions([self._rec()], self.ps, self.live_panes)
        self.assertEqual(set(got), {"%19"})
        self.assertEqual(got["%19"]["sessionId"], "cbf3c6a9-a5b7-45fc-a727-c20db56cf5d7")
        self.assertEqual(got["%19"]["args"], ["--chrome"])

    def test_rejects_dead_pid(self):
        rec = self._rec(pid=2603, procStart="Mon Aug 17 20:00:00 2026")
        self.assertEqual(
            tcs.select_live_claude_sessions([rec], self.ps, self.live_panes), {}
        )

    def test_rejects_recycled_pid_with_different_start_time(self):
        rec = self._rec(procStart="Sun Aug 16 01:00:00 2026")
        self.assertEqual(
            tcs.select_live_claude_sessions([rec], self.ps, self.live_panes), {}
        )

    def test_rejects_pane_that_no_longer_exists(self):
        rec = self._rec(tmux="default:@99.%99")
        self.assertEqual(
            tcs.select_live_claude_sessions([rec], self.ps, self.live_panes), {}
        )

    def test_rejects_same_pane_id_in_a_different_session(self):
        rec = self._rec(tmux="other:@15.%19")
        self.assertEqual(
            tcs.select_live_claude_sessions([rec], self.ps, self.live_panes), {}
        )

    def test_rejects_same_pane_id_under_a_different_window(self):
        rec = self._rec(tmux="default:@99.%19")
        self.assertEqual(
            tcs.select_live_claude_sessions([rec], self.ps, self.live_panes), {}
        )

    def test_rejects_non_interactive_kind(self):
        rec = self._rec(kind="print")
        self.assertEqual(
            tcs.select_live_claude_sessions([rec], self.ps, self.live_panes), {}
        )

    def test_keeps_newest_when_two_records_share_a_pane(self):
        old = self._rec(sessionId="old", startedAt=100)
        new = self._rec(
            pid=1015,
            sessionId="new",
            procStart="Tue Aug 18 10:22:25 2026",
            startedAt=200,
        )
        got = tcs.select_live_claude_sessions([old, new], self.ps, self.live_panes)
        self.assertEqual(got["%19"]["sessionId"], "new")


class TestPaneKey(unittest.TestCase):
    def test_joins_session_window_and_pane(self):
        self.assertEqual(tcs.pane_key("default", "@22", "%28"), "default:@22.%28")

    def test_round_trips_with_parse_tmux_ref(self):
        ref = "default:@22.%28"
        self.assertEqual(tcs.pane_key(*tcs.parse_tmux_ref(ref)), ref)


class TestBuildSnapshot(unittest.TestCase):
    def test_groups_panes_into_windows_and_sessions(self):
        panes = [
            {"session": "default", "window_id": "@1", "window_index": "0",
             "window_name": "w0", "window_active": "1",
             "window_layout": "abcd,10x10,0,0,1", "pane_id": "%1",
             "pane_index": "0", "pane_active": "1", "cwd": "/a",
             "command": "zsh", "title": "t1"},
            {"session": "default", "window_id": "@1", "window_index": "0",
             "window_name": "w0", "window_active": "1",
             "window_layout": "abcd,10x10,0,0,1", "pane_id": "%2",
             "pane_index": "1", "pane_active": "0", "cwd": "/b",
             "command": "2.1.233", "title": "t2"},
            {"session": "other", "window_id": "@5", "window_index": "3",
             "window_name": "w3", "window_active": "0",
             "window_layout": "efgh,10x10,0,0,5", "pane_id": "%5",
             "pane_index": "0", "pane_active": "1", "cwd": "/c",
             "command": "zsh", "title": "t5"},
        ]
        claude = {"%2": {"sessionId": "sid-2", "args": ["--chrome"]}}
        snap = tcs.build_snapshot(panes, claude)
        names = [s["name"] for s in snap["sessions"]]
        self.assertEqual(names, ["default", "other"])
        default = snap["sessions"][0]
        self.assertEqual(len(default["windows"]), 1)
        win = default["windows"][0]
        self.assertEqual(win["index"], 0)
        self.assertEqual(win["layout"], "abcd,10x10,0,0,1")
        self.assertEqual([p["cwd"] for p in win["panes"]], ["/a", "/b"])
        self.assertIsNone(win["panes"][0]["claude"])
        self.assertEqual(win["panes"][1]["claude"]["sessionId"], "sid-2")
        self.assertEqual(snap["claudeSessionCount"], 1)

    def test_windows_are_sorted_by_index(self):
        def pane(win_index, pane_id):
            return {"session": "s", "window_id": "@" + win_index,
                    "window_index": win_index, "window_name": "w",
                    "window_active": "0", "window_layout": "l", "pane_id": pane_id,
                    "pane_index": "0", "pane_active": "1", "cwd": "/",
                    "command": "zsh", "title": ""}
        snap = tcs.build_snapshot([pane("10", "%1"), pane("2", "%2")], {})
        self.assertEqual([w["index"] for w in snap["sessions"][0]["windows"]], [2, 10])


class TestSnapshotChanged(unittest.TestCase):
    def _snap(self, saved_at="t1", name="default", title="x", cwd="/a",
              sid="sid-1", layout="abcd"):
        return {"version": 1, "savedAt": saved_at, "claudeSessionCount": 1,
                "sessions": [{"name": name, "windows": [
                    {"index": 0, "name": "w", "layout": layout, "active": True,
                     "panes": [{"index": 0, "cwd": cwd, "command": "2.1.1",
                                "title": title, "active": True,
                                "claude": {"sessionId": sid, "args": []}}]}]}]}

    def test_same_content_with_different_timestamp_is_unchanged(self):
        self.assertFalse(
            tcs.snapshot_changed(self._snap("t1"), self._snap("t2"))
        )

    def test_spinner_in_pane_title_is_not_a_change(self):
        # claude のスピナーで pane_title が数秒ごとに変わるため構成差分に数えない
        self.assertFalse(
            tcs.snapshot_changed(self._snap(title="◑ 作業中"),
                                 self._snap(title="◐ 作業中"))
        )

    def test_different_session_name_is_changed(self):
        self.assertTrue(
            tcs.snapshot_changed(self._snap(), self._snap(name="other"))
        )

    def test_different_cwd_is_changed(self):
        self.assertTrue(tcs.snapshot_changed(self._snap(), self._snap(cwd="/b")))

    def test_different_claude_session_is_changed(self):
        self.assertTrue(tcs.snapshot_changed(self._snap(), self._snap(sid="sid-2")))

    def test_different_layout_is_changed(self):
        self.assertTrue(tcs.snapshot_changed(self._snap(), self._snap(layout="efgh")))

    def test_missing_previous_is_changed(self):
        self.assertTrue(tcs.snapshot_changed(None, self._snap("t1")))


class TestResolveSessionName(unittest.TestCase):
    def test_uses_saved_name_when_free(self):
        self.assertEqual(tcs.resolve_session_name("default", set()), "default")

    def test_adds_suffix_when_zsh_already_made_the_session(self):
        self.assertEqual(
            tcs.resolve_session_name("default", {"default"}), "default-2"
        )

    def test_counts_up_past_earlier_restores(self):
        self.assertEqual(
            tcs.resolve_session_name("default", {"default", "default-2"}),
            "default-3",
        )

    def test_unrelated_sessions_do_not_shift_the_name(self):
        self.assertEqual(
            tcs.resolve_session_name("default", {"other", "default-2"}), "default"
        )


class TestDropRunningClaudeSessions(unittest.TestCase):
    def _win(self, index, *sids):
        return {"index": index, "name": "w", "layout": "l", "active": False,
                "panes": [{"index": i, "cwd": "/a", "command": "c", "title": "",
                           "active": i == 0,
                           "claude": {"sessionId": sid, "args": []} if sid else None}
                          for i, sid in enumerate(sids)]}

    def _snap(self, *windows):
        return {"sessions": [{"name": "default", "windows": list(windows)}]}

    def test_removes_window_whose_only_claude_is_already_running(self):
        snap = self._snap(self._win(0, "live"), self._win(1, "stale"))
        dropped = tcs.drop_running_claude_sessions(snap, {"live"})
        self.assertEqual(dropped, ["live"])
        left = snap["sessions"][0]["windows"]
        self.assertEqual([w["index"] for w in left], [1])

    def test_keeps_window_when_only_some_panes_are_running(self):
        snap = self._snap(self._win(0, "live", "stale"))
        self.assertEqual(tcs.drop_running_claude_sessions(snap, {"live"}), ["live"])
        panes = snap["sessions"][0]["windows"][0]["panes"]
        self.assertIsNone(panes[0]["claude"])
        self.assertEqual(panes[1]["claude"]["sessionId"], "stale")

    def test_keeps_plain_shell_windows(self):
        snap = self._snap(self._win(0, None))
        self.assertEqual(tcs.drop_running_claude_sessions(snap, {"live"}), [])
        self.assertEqual(len(snap["sessions"][0]["windows"]), 1)

    def test_keeps_everything_when_nothing_is_running(self):
        snap = self._snap(self._win(0, "a"), self._win(1, "b"))
        self.assertEqual(tcs.drop_running_claude_sessions(snap, set()), [])
        self.assertEqual(len(snap["sessions"][0]["windows"]), 2)


class TestExtractPendingResume(unittest.TestCase):
    SID = "0bfdb96d-929f-4edd-89b2-449339ee858c"

    def test_reads_a_restored_but_unstarted_pane(self):
        # 復元直後は claude プロセスが無いので、入力済みコマンドから拾う
        got = tcs.extract_pending_resume(
            "~/src/org-files\n$ claude --chrome --resume %s" % self.SID)
        self.assertEqual(got, {"sessionId": self.SID, "args": ["--chrome"]})

    def test_ignores_trailing_blank_lines(self):
        got = tcs.extract_pending_resume(
            "$ claude --resume %s\n\n   \n" % self.SID)
        self.assertEqual(got, {"sessionId": self.SID, "args": []})

    def test_ignores_older_history_lines(self):
        # 最後の非空行だけを見る。履歴に残った古い復元コマンドは拾わない
        self.assertIsNone(tcs.extract_pending_resume(
            "$ claude --resume %s\n$ ls\n$ git status" % self.SID))

    def test_returns_none_without_a_resume_command(self):
        self.assertIsNone(tcs.extract_pending_resume("$ ls -la"))
        self.assertIsNone(tcs.extract_pending_resume(""))

    def test_returns_none_for_a_malformed_session_id(self):
        self.assertIsNone(tcs.extract_pending_resume("$ claude --resume nope"))

    def test_survives_unbalanced_quotes(self):
        self.assertIsNone(tcs.extract_pending_resume('$ echo "claude --resume '))


class TestResolveHistoryStamp(unittest.TestCase):
    STAMPS = ["20260818-100000", "20260818-103036", "20260819-090000"]

    def test_exact_stamp(self):
        self.assertEqual(
            tcs.resolve_history_stamp("20260818-103036", self.STAMPS),
            "20260818-103036",
        )

    def test_prefix_picks_the_newest_match(self):
        self.assertEqual(
            tcs.resolve_history_stamp("20260818", self.STAMPS), "20260818-103036"
        )

    def test_latest_keyword_picks_the_newest_overall(self):
        self.assertEqual(
            tcs.resolve_history_stamp("latest", self.STAMPS), "20260819-090000"
        )

    def test_unknown_stamp_returns_none(self):
        self.assertIsNone(tcs.resolve_history_stamp("20991231", self.STAMPS))
        self.assertIsNone(tcs.resolve_history_stamp("latest", []))


class TestLastNonemptyLine(unittest.TestCase):
    def test_picks_the_last_line_with_content(self):
        self.assertEqual(tcs.last_nonempty_line("a\nb\n\n   \n"), "b")

    def test_strips_surrounding_space(self):
        self.assertEqual(tcs.last_nonempty_line("  $ claude  "), "$ claude")

    def test_empty_input(self):
        self.assertEqual(tcs.last_nonempty_line(""), "")
        self.assertEqual(tcs.last_nonempty_line(None), "")


if __name__ == "__main__":
    unittest.main()
