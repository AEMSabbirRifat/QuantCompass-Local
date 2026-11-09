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
    jQuery(document).on("shiny:value", function (e) { if (e.name === "qc_footer") setTimeout(measure, 0); });
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
