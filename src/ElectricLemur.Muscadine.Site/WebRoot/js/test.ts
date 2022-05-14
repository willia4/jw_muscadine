
interface TestInterface {
    name: string;
    data?: string;
}

function showTestData(data: TestInterface) {
    let msg = "Name: name";
    if (data.data) {
        msg = `${msg}; Data: ${data.data}`;
    }
    alert(msg);
}

function showMoreTestData(data: TestInterface) {
    data.data = "Hello Bob (or is it Robert?)";
    showTestData(data);
}