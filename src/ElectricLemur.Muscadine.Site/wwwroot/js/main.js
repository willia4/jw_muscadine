function formatUtcDate(dateString) {
  const padNumber = (n => n < 10 ? "0" + n : "" + n);

  const d = new Date(dateString);
  if (d.toString() === "Invalid Date") { return "??date"; }

  const amPm = d.getHours() >= 12 ? "pm" : "am";

  const year = d.getFullYear();
  const month = padNumber(d.getMonth() + 1);
  const day = padNumber(d.getDate());
  const hour = padNumber(d.getHours() % 12);
  const minute = padNumber(d.getMinutes());

  return "" + year + "-" + month + "-" + day + " @ " + hour + ":" + minute + amPm;
}

function toggleMenu() {
  const sidebar = document.getElementById("main-sidebar");
  if (!sidebar) { return; }
  const sidebarClasses = Array.from(sidebar.classList)

  if (sidebarClasses.indexOf('shown') >= 0) {
    sidebar.classList.remove('shown');
  }
  else {
    sidebar.classList.add('shown');
  }
}

(() => {
  if (document.addEventListener) {
      document.addEventListener("DOMContentLoaded", () => {
          const menuButton = Array.from(document.getElementsByClassName("menu-button"));
          menuButton.forEach(b => b.addEventListener("click", (event) => { toggleMenu(); }));
          ;
    });
  }
})();
