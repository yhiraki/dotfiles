settings.blacklistPattern =
  /.*mail.google.com.*|.*inbox.google.com.*|trello.com|duolingo.com|youtube.com|udemy.com|localhost/i;

// Prevent automatic next/previous page loads
settings.smartPageBoundary = false;

api.map("d", "x");
api.map("u", "X");
api.map("P", "sg");
api.map("<Ctrl-i>", "D");
api.map("<Ctrl-o>", "S");

api.iunmap(":");
api.iunmap("<ctrl-a>");
api.iunmap("<ctrl-e>");
api.iunmap("<ctrl-b>");
api.iunmap("<ctrl-f>");
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

const getPageMetaCurent = () => {
  return {
    url: location.href,
    title: document.title,
  };
};

const getPageMetaGithub = () => {
  const url = new URL(location.href);
  const d = {};
  if (url.pathname.split("/").length >= 3)
    d.repoName = url.pathname.split("/").slice(1, 3).join("/");
  if (url.pathname.includes("/pull/")) d.prNo = url.pathname.split("/").pop();
  d.issueTitle = document.getElementById("issue_title")?.value;
  return d;
};

api.mapkey("yfm", "copy markdown style link", () => {
  const { url, title } = getPageMetaCurent();
  api.Clipboard.write(`[${title}](${url})`);
});

api.mapkey("yfo", "copy orgmode sytle link", () => {
  const { url, title } = getPageMetaCurent();
  const u = new URL(url);
  if (u.host == "github.com") {
    if (u.pathname.includes("pull")) {
      const { prNo, repoName, issueTitle } = getPageMetaGithub();
      api.Clipboard.write(`${repoName} [[${url}][#${prNo}]] ${issueTitle}`);
      return;
    }
    // pass through
  }
  api.Clipboard.write(`[[${url}][${title}]]`);
});

api.mapkey(
  "yfO",
  "copy list of pull requests orgmode",
  () => {
    const links = Array.from(
      document.querySelectorAll("[data-hovercard-type=pull_request]")
    ).map((v) => {
      const l = v.href.split("/");
      const prNo = l.slice(-1);
      const repoName = l.slice(-4, -2).join("/");
      return `${repoName} [[${v.href}][#${prNo}]] ${v.textContent}`;
    });
    api.Clipboard.write(links.join("\n"));
  },
  /github.com\/pulls/
);

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

api.unmapAllExcept(
  ["E", "R", "T", "b", "cf", "f", "r", "t"],
  /mail.google.com/
);
