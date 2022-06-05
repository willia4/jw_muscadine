async function deleteItemClick(event: MouseEvent, button: HTMLButtonElement) {
    const idAttribute = button.attributes.getNamedItem("data-id");
    const id = idAttribute ? idAttribute.value : null;

    const nameAttribute = button.attributes.getNamedItem("data-name");
    const name = nameAttribute ? nameAttribute.value : null;

    const urlAttribute = button.attributes.getNamedItem("data-url");
    const url = urlAttribute ? urlAttribute.value : null;

    if (!(!!id && !!name && !!url)) {
        alert(`Invalid page state: could not determine item to delete`);
        return;
    }

    const answer = confirm(`Do you want to delete the item ${name}?`);
    if (!answer) return;

    const response = await fetch(url, {
        method: "DELETE",
        mode: "same-origin",
        cache: "no-cache",
        credentials: "same-origin"
    });

    if (!response || response.status !== 200) {
        alert("Could not delete item; response was unsuccessful");
    }

    location.reload();
}

(() => {
    if (document.addEventListener) {
        document.addEventListener("DOMContentLoaded", () => {
            const buttons = document.getElementsByClassName("delete-button");

            for (let i = 0; i < buttons.length; i++) {
                const b = buttons[i] as HTMLButtonElement;
                
                b.addEventListener("click", (event: MouseEvent) => {
                    deleteItemClick(event, b);
                });
            }
        });
    }
})();