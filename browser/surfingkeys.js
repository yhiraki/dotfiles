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

const copyTitleAndUrl = (format) => {
  const text = format
    .replace("{URL}", location.href)
    .replace("{TITLE}", document.title);
  api.Clipboard.write(text);
};

api.mapkey("yfm", "copy markdown style link", () => {
  copyTitleAndUrl("[{TITLE}]({URL})");
});

api.mapkey("yfo", "copy orgmode sytle link", () => {
  copyTitleAndUrl("[[{URL}][{TITLE}]]");
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
