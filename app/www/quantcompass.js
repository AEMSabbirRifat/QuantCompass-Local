// QuantCompass interface behaviour: fixed workspace layout, step markers in the top bar, the AI help drawer,
// the summary panel toggle, and the compass needle that follows the cursor (off when reduced motion is preferred).
window.QC = window.QC || {};
(function () {
  var calm = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  // ---- layout: the work area fills the window between the top bar and the Back/Continue bar
  function measure() {
    var nav = document.querySelector(".navbar"), foot = document.getElementById("qc-footer");
    var root = document.documentElement.style;
    if (nav) root.setProperty("--qc-top", nav.offsetHeight + "px");
    if (foot) root.setProperty("--qc-foot", foot.offsetHeight + "px");
  }
  window.addEventListener("resize", measure);

  // ---- move the AI and Summary buttons into the top bar
  function placeTools() {
    var tools = document.getElementById("qc-tools"), bar = document.querySelector(".navbar > .container-fluid, .navbar > .container");
    if (tools && bar && tools.parentNode !== bar) bar.appendChild(tools);
  }

  // ---- summary panel on/off (remembered in this browser)
  function setSummary(on) {
    document.body.classList.toggle("qc-sum-off", !on);
    try { localStorage.setItem("qc-summary", on ? "on" : "off"); } catch (e) {}
    var b = document.getElementById("qc-sum-toggle"); if (b) b.classList.toggle("active", on);
  }

  // ---- the side panel: "Your selections" or the AI chat
  QC.panel = function (which) {
    var chat = which === "chat";
    document.body.classList.toggle("qc-chat-open", chat); document.body.classList.remove("qc-sum-off");
    var a = document.getElementById("qs-tab-sel"), b = document.getElementById("qs-tab-chat");
    if (a) a.classList.toggle("active", !chat); if (b) b.classList.toggle("active", chat);
    var t = document.getElementById("qc-sum-toggle"); if (t) t.classList.add("active");
    measure(); if (chat) { var ta = document.getElementById("qc-chat-text"); if (ta) setTimeout(function () { ta.focus(); }, 50); QC.scrollChat(); }
  };
  QC.scrollChat = function () { var m = document.getElementById("qc-chat-msgs"); if (m) setTimeout(function () { m.scrollTop = m.scrollHeight; }, 30); };
  QC.ask = function (text) {
    if (!text) return; QC.panel("chat");
    if (window.Shiny) Shiny.setInputValue("chat_send", { text: text, n: Date.now() }, { priority: "event" });
  };
  QC.send = function () {
    var ta = document.getElementById("qc-chat-text"); if (!ta) return;
    var v = ta.value.trim(); if (!v) { ta.focus(); return; }
    ta.value = ""; QC.ask(v);
  };
  QC.askFrom = function (id, prefix) {
    var el = document.getElementById(id); if (!el) return;
    var v = el.value.trim(); if (!v) { el.focus(); return; }
    el.value = ""; QC.ask((prefix || "") + v);
  };
  QC.act = function (i, what, j) { Shiny.setInputValue("chat_act", { i: i, what: what, j: j, n: Date.now() }, { priority: "event" }); };
  // a scale edited in its "How it is measured" panel
  QC.scaleEdit = function (btn, name) {
    var box = btn.closest(".qc-measure"); if (!box) return;
    var pick = function (sel) { return Array.prototype.map.call(box.querySelectorAll(sel), function (x) { return x.getAttribute("data-item"); }); };
    var ad = box.querySelector(".qc-m-add"); var add = ad && ad.value ? [ad.value] : [];
    var m = box.querySelector(".qc-m-method");
    Shiny.setInputValue("sc_edit", { name: name, use: pick(".qc-it-use:checked"), rev: pick(".qc-it-rev:checked"), add: add, method: m ? m.value : "mean", n: Date.now() }, { priority: "event" });
  };
  document.addEventListener("keydown", function (e) {
    if (e.target && e.target.id === "qc-chat-text" && e.key === "Enter" && !e.shiftKey) { e.preventDefault(); QC.send(); }
  });

  QC.openAI = function () {
    var el = document.getElementById("qc-ai");
    if (el && window.bootstrap) bootstrap.Offcanvas.getOrCreateInstance(el).show();
  };

  document.addEventListener("DOMContentLoaded", function () {
    placeTools();
    var on = true; try { on = localStorage.getItem("qc-summary") !== "off"; } catch (e) {}
    setSummary(on);
    var t = document.getElementById("qc-sum-toggle");
    if (t) t.addEventListener("click", function () { setSummary(document.body.classList.contains("qc-sum-off")); });
    measure(); setTimeout(measure, 300);
  });

  if (window.jQuery) {
    jQuery(document).on("shiny:connected", function () {
      Shiny.addCustomMessageHandler("qc_open_ai", function (m) { QC.openAI(); });
      // mark steps in the top bar: done / to do / optional / locked
      Shiny.addCustomMessageHandler("qc_steps", function (m) {
        var st = m.state || {};
        document.querySelectorAll(".navbar-nav a[data-value]").forEach(function (a) {
          var v = a.getAttribute("data-value"), s = st[v];
          ["done", "todo", "optional", "locked"].forEach(function (c) { a.classList.toggle("qc-" + c, s === c); });
        });
      });
      measure();
    });
    // a new step starts at the top of its panes; the footer may change height
    jQuery(document).on("shown.bs.tab", function () { measure(); });
    jQuery(document).on("shiny:value", function (e) { if (e.name === "qc_footer") setTimeout(measure, 0); if (e.name === "chat_msgs") QC.scrollChat(); });
  }

  // ---- compass needle follows the cursor
  var turned = 0;
  document.addEventListener("mousemove", function (e) {
    if (calm) return;
    var needle = document.querySelector(".qc-needle");
    if (!needle) return;
    var r = needle.ownerSVGElement.getBoundingClientRect();
    var ang = Math.atan2(e.clientY - (r.top + r.height * 29 / 64), e.clientX - (r.left + r.width * 29 / 64)) * 180 / Math.PI + 90;
    var d = ((ang - turned) % 360 + 540) % 360 - 180;   // shortest way round, no spinning back
    turned += d;
    needle.style.transform = "rotate(" + turned + "deg)";
  }, { passive: true });
})();
