settings.blacklistPattern =
  /.*mail.google.com.*|.*inbox.google.com.*|trello.com|duolingo.com|youtube.com|udemy.com|localhost/i;

// Prevent automatic next/previous page loads
settings.smartPageBoundary = false;

api.map("d", "x");
api.unmap("x");
api.map("u", "X");
api.unmap("X");
api.map("<Ctrl-o>", "S");
api.unmap("S");
api.map("<Ctrl-i>", "D");
api.unmap("D");
api.map("P", "sg");

const copyTitleAndUrl = (format) => {
  const text = format
    .replace("{URL}", location.href)
    .replace("{TITLE}", document.title);
  api.Clipboard.write(text);
};

api.unmap("yf");

api.mapkey("yfm", "copy markdown style link", () => {
  copytitleandurl("[{title}]({url})");
});

api.mapkey("yfo", "copy orgmode sytle link", () => {
  copytitleandurl("[[{url}][{title}]]");
});

api.mapkey("yfO", "copy list of pull requests orgmode", () => {
  const links = Array.from(
    document.querySelectorAll("[data-hovercard-type=pull_request]")
  ).map((v) => {
    const prNo = v.href.split("/").pop();
    return `[[${v.href}][#${prNo}]] ${v.textContent}`;
  });
  api.Clipboard.write(links.join("\n"));
}, /github.com\/pulls/);

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

api.unmapAllExcept(["E", "R", "T", "b", "f", "r", "t"], /mail.google.com/);
