// import createAPI from "./Surfingkeys/src/content_scripts/common/api.js";
// const api = createAPI();
// let settings: any;

settings.blacklistPattern =
  /.*mail.google.com.*|.*inbox.google.com.*|trello.com|duolingo.com|youtube.com|udemy.com|localhost/i;

// Prevent automatic next/previous page loads
settings.smartPageBoundary = false;

// click `save` button to make above settings to take effect.
// set theme
settings.theme =
  "\
.sk_theme { \
    background: #fff; \
    color: #000; \
} \
.sk_theme tbody { \
    color: #000; \
} \
.sk_theme input { \
    color: #000; \
} \
.sk_theme .url { \
    color: #555; \
} \
.sk_theme .annotation { \
    color: #555; \
} \
.sk_theme .focused { \
    background: #f0f0f0; \
}";

settings.smoothScroll = false;

// Always use omnibar to select tabs
settings.tabsThreshold = 0;

api.map("d", "x");
api.map("u", "X");
api.map("P", "sg");
api.map("<Ctrl-i>", "D");
api.map("<Ctrl-o>", "S");

api.iunmap(":");
api.iunmap("<Alt-b>");
api.iunmap("<Alt-d>");
api.iunmap("<Alt-f>");
api.iunmap("<Alt-w>");
api.iunmap("<Ctrl-'>");
api.iunmap("<Ctrl-a>");
api.iunmap("<Ctrl-b>");
api.iunmap("<Ctrl-e>");
api.iunmap("<Ctrl-f>");
api.iunmap("<Ctrl-u>");
api.iunmap("<Esc>");

api.unmap("D");
api.unmap("S");
api.unmap("X");
api.unmap("ob");
api.unmap("oe");
api.unmap("os");
api.unmap("ow");
api.unmap("oy");
api.unmap("x");
api.unmap("yf");

const parsePageCurent = () => {
  return {
    url: location.href,
    title: document.title,
  };
};

const parsePageGithub = () => {
  return {
    issueTitle: document.querySelector(".markdown_title")?.value,
  };
};

const parseURLGithub = (rawUrl = location.href) => {
  const url = new URL(rawUrl);
  return {
    repoName: url.pathname.split("/").slice(1, 3).join("/"),
    issueNo: Number(url.pathname.split("/").pop()),
  };
};

api.mapkey("yfm", "copy markdown style link", () => {
  const { url, title } = parsePageCurent();
  api.Clipboard.write(`[${title}](${url})`);
});

api.mapkey("yfo", "copy orgmode sytle link", () => {
  const { url, title } = parsePageCurent();
  api.Clipboard.write(`[[${url}][${title}]]`);
});

api.mapkey(
  "yfo",
  "[GitHub PR/Issues] copy orgmode sytle link",
  () => {
    const { url } = parsePageCurent();
    const { issueTitle } = parsePageGithub();
    const { issueNo, repoName } = parseURLGithub();
    api.Clipboard.write(`${repoName} [[${url}][#${issueNo}]] ${issueTitle}`);
  },
  { domain: /github\.com\/.*\/(pull|issues)\/\d+/ }
);

api.mapkey(
  "yfO",
  "copy list of PR/Issues orgmode",
  () => {
    const links = Array.from(document.querySelectorAll("a"))
      .filter((v) => /\/(pull|issues)\/\d+$/.test(v.href))
      .sort((a, b) => Number(a.href > b.href))
      .filter((v, i, arr) => i > 0 && v.href !== arr[i - 1].href)
      .map((v) => {
        const l = v.href.split("/");
        const no = l.slice(-1);
        const repoName = l.slice(-4, -2).join("/");
        return `${repoName} [[${v.href}][#${no}]] ${v.textContent}`;
      });
    api.Clipboard.write(links.join("\n"));
  },
  { domain: /github\.com\/(pulls|issues)$/ }
);

api.unmapAllExcept(
  ["E", "R", "T", "b", "cf", "f", "r", "t"],
  /mail.google.com/
);
