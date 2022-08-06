function formatUtcDate(dateString) {
  const d = new Date(dateString);
  if (d.toString() === "Invalid Date") { return "??date"; }

  const amPm = d.getHours() >= 12 ? "pm" : "am";
  const hours = d.getHours() % 12;
  const formattedHours = hours < 10 ? "0" + hours : "" + hours
  const formattedMinutes = d.getMinutes() < 10 ? "0" + d.getMinutes() : "" + d.getMinutes();

  return "" + d.getFullYear() + "-" + (d.getMonth() + 1) + "-" + d.getDate() + " @ " + formattedHours + ":" + formattedMinutes + amPm;
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
