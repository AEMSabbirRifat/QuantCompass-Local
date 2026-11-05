// QuantCompass interface behaviour: compass needle follows the cursor, light follows the cursor
// on panels, ripple on buttons. All of it is switched off when the user prefers reduced motion.
(function () {
  var calm = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var turned = 0;
  document.addEventListener("mousemove", function (e) {
    if (calm) return;
    var needle = document.querySelector(".qc-needle");
    if (needle) {
      var r = needle.ownerSVGElement.getBoundingClientRect();
      var ang = Math.atan2(e.clientY - (r.top + r.height * 29 / 64), e.clientX - (r.left + r.width * 29 / 64)) * 180 / Math.PI + 90;
      var d = ((ang - turned) % 360 + 540) % 360 - 180;   // shortest way round, no spinning back
      turned += d;
      needle.style.transform = "rotate(" + turned + "deg)";
    }
    var card = e.target.closest && e.target.closest(".card");
    if (card) {
      var c = card.getBoundingClientRect();
      card.style.setProperty("--mx", (e.clientX - c.left) + "px");
      card.style.setProperty("--my", (e.clientY - c.top) + "px");
    }
  }, { passive: true });

  document.addEventListener("click", function (e) {
    var b = e.target.closest && e.target.closest(".btn, .idea");
    if (!b || calm) return;
    var r = b.getBoundingClientRect(), s = Math.max(r.width, r.height);
    var span = document.createElement("span");
    span.className = "qc-ripple";
    span.style.width = span.style.height = s + "px";
    span.style.left = (e.clientX - r.left - s / 2) + "px";
    span.style.top = (e.clientY - r.top - s / 2) + "px";
    b.appendChild(span);
    setTimeout(function () { span.remove(); }, 650);
  });
})();
