async function fetchStudents(){
  const resp = await fetch('/api/students');
  if(!resp.ok) { console.error('Failed to fetch students'); return []; }
  const arr = await resp.json();
  return arr;
}

async function fetchReport(){
  const resp = await fetch('/api/report');
  if(!resp.ok) { console.error('Failed to fetch report'); return null }
  return await resp.json();
}

function renderStudents(list){
  const tbody = document.querySelector('#students tbody');
  tbody.innerHTML = '';
  list.forEach((s, i) => {
    const tr = document.createElement('tr');
    const marks = (s.marks || []).join(', ');
    tr.innerHTML = `<td>${i+1}</td><td>${s.sid}</td><td>${s.name}</td><td>${marks}</td><td>${s.average.toFixed(1)}</td><td>${s.max}</td><td>${s.min}</td><td>${s.grade}</td>`;
    tbody.appendChild(tr);
  });
}

async function refresh(){
  const students = await fetchStudents();
  renderStudents(students);
  const report = await fetchReport();
  if(report){
    document.getElementById('count').textContent = `  (Students: ${report.count})`;
    document.getElementById('cohort').textContent = `Cohort average: ${report.cohortAverage.toFixed(1)}`;
  }
}

document.getElementById('refresh').addEventListener('click', refresh);
window.addEventListener('load', refresh);
