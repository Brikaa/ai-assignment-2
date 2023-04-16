(async () => {
    const form = document.getElementsByTagName('form')[0];
    form.addEventListener('submit', async (e) => {
        e.preventDefault();
        const formData = Object.fromEntries(new FormData(form));
        const requestBody = {
            rows: parseInt(formData.rows),
            columns: parseInt(formData.columns),
            bomb1: {
                row: parseInt(formData.bomb1Row),
                column: parseInt(formData.bomb1Column)
            },
            bomb2: {
                row: parseInt(formData.bomb2Row),
                column: parseInt(formData.bomb2Column)
            }
        };

        const headers = new Headers();
        headers.append('Content-Type', 'application/json');
        const raw = JSON.stringify(requestBody);
        const requestOptions = {
            method: 'POST',
            headers,
            body: raw
        };
        const response = await (
            await fetch('http://localhost:2005/uninformed', requestOptions)
        ).json();
        console.log(response);
    });
})();
