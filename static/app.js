// Global state
let allStudents = [];
let filteredStudents = [];
let currentSort = { column: null, ascending: true };

// Fetch students from API
async function fetchStudents() {
  try {
    const resp = await fetch('/api/students');
    if (!resp.ok) {
      throw new Error('Failed to fetch students');
    }
    return await resp.json();
  } catch (error) {
    showError('Error fetching students: ' + error.message);
    return [];
  }
}

// Fetch report summary
async function fetchReport() {
  try {
    const resp = await fetch('/api/report');
    if (!resp.ok) {
      throw new Error('Failed to fetch report');
    }
    return await resp.json();
  } catch (error) {
    showError('Error fetching report: ' + error.message);
    return null;
  }
}

// Display error message
function showError(message) {
  const container = document.getElementById('error-container');
  const errorDiv = document.createElement('div');
  errorDiv.className = 'error';
  errorDiv.textContent = message;
  container.innerHTML = '';
  container.appendChild(errorDiv);
  
  // Auto-hide after 5 seconds
  setTimeout(() => {
    errorDiv.remove();
  }, 5000);
}

// Render students table
function renderStudents(students) {
  const tbody = document.getElementById('students-tbody');
  
  if (!students || students.length === 0) {
    tbody.innerHTML = '<tr><td colspan="8" class="no-results">No students found</td></tr>';
    return;
  }

  tbody.innerHTML = '';
  students.forEach((student, index) => {
    const tr = document.createElement('tr');
    const marks = (student.marks || []).join(', ');
    const gradeClass = `grade-${student.grade}`;
    
    tr.innerHTML = `
      <td>${index + 1}</td>
      <td>${student.sid}</td>
      <td>${student.name}</td>
      <td>${marks}</td>
      <td><strong>${parseFloat(student.average).toFixed(2)}</strong></td>
      <td>${student.max}</td>
      <td>${student.min}</td>
      <td><span class="grade-badge ${gradeClass}">${student.grade}</span></td>
    `;
    tbody.appendChild(tr);
  });
}

// Update statistics
function updateStats(students, report) {
  // Update counts
  document.getElementById('total-count').textContent = report.count || 0;
  document.getElementById('cohort-avg').textContent = 
    report.cohortAverage ? parseFloat(report.cohortAverage).toFixed(2) : '0.00';

  // Calculate highest and lowest
  if (students.length > 0) {
    const averages = students.map(s => s.average);
    const highest = Math.max(...averages);
    const lowest = Math.min(...averages);
    
    document.getElementById('highest-avg').textContent = highest.toFixed(2);
    document.getElementById('lowest-avg').textContent = lowest.toFixed(2);
  }
}

// Update grade distribution
function updateGradeDistribution(report) {
  if (!report || !report.gradeDistribution) return;

  const gradeCounts = {};
  let totalGrades = 0;
  
  report.gradeDistribution.forEach(item => {
    gradeCounts[item.grade] = item.count;
    totalGrades += item.count;
  });

  // Update grade badges
  ['A', 'B', 'C', 'D', 'F'].forEach(grade => {
    const count = gradeCounts[grade] || 0;
    document.getElementById(`count-${grade}`).textContent = count;
  });

  // Update grade summary table
  const tbody = document.getElementById('grade-summary-tbody');
  tbody.innerHTML = '';
  
  ['A', 'B', 'C', 'D', 'F'].forEach(grade => {
    const count = gradeCounts[grade] || 0;
    const percentage = totalGrades > 0 ? ((count / totalGrades) * 100).toFixed(1) : '0.0';
    
    const tr = document.createElement('tr');
    tr.innerHTML = `
      <td><span class="grade-badge grade-${grade}">${grade}</span></td>
      <td>${count}</td>
      <td>${percentage}%</td>
    `;
    tbody.appendChild(tr);
  });
}

// Filter students based on search
function filterStudents() {
  const searchInput = document.getElementById('search-input').value.toLowerCase();
  
  if (searchInput === '') {
    filteredStudents = [...allStudents];
  } else {
    filteredStudents = allStudents.filter(student =>
      student.sid.toLowerCase().includes(searchInput) ||
      student.name.toLowerCase().includes(searchInput)
    );
  }
  
  renderStudents(filteredStudents);
}

// Sort table by column
function sortTable(columnIndex) {
  const columnHeaders = ['#', 'sid', 'name', 'marks', 'average', 'max', 'min', 'grade'];
  const columnName = columnHeaders[columnIndex];

  // Toggle sort direction if same column clicked
  if (currentSort.column === columnName) {
    currentSort.ascending = !currentSort.ascending;
  } else {
    currentSort.column = columnName;
    currentSort.ascending = true;
  }

  // Sort the filtered students
  filteredStudents.sort((a, b) => {
    let aVal = a[columnName];
    let bVal = b[columnName];

    // Handle numeric columns
    if (columnIndex === 4 || columnIndex === 5 || columnIndex === 6) {
      aVal = parseFloat(aVal);
      bVal = parseFloat(bVal);
    }

    let comparison = 0;
    if (typeof aVal === 'string') {
      comparison = aVal.localeCompare(bVal);
    } else {
      comparison = aVal < bVal ? -1 : aVal > bVal ? 1 : 0;
    }

    return currentSort.ascending ? comparison : -comparison;
  });

  renderStudents(filteredStudents);
  updateSortIndicators(columnIndex);
}

// Update sort indicators in table headers
function updateSortIndicators(activeColumn) {
  const headers = document.querySelectorAll('th');
  headers.forEach((header, index) => {
    const indicator = header.querySelector('.sort-indicator');
    if (indicator) {
      if (index === activeColumn) {
        indicator.textContent = currentSort.ascending ? ' ▲' : ' ▼';
      } else {
        indicator.textContent = '';
      }
    }
  });
}

// Export report as text file
function exportReport() {
  if (allStudents.length === 0) {
    showError('No data to export');
    return;
  }

  let reportContent = 'Student Marks Analyzer Report\n';
  reportContent += '================================\n\n';
  reportContent += `Generated: ${new Date().toLocaleString()}\n`;
  reportContent += `Total Students: ${allStudents.length}\n`;
  reportContent += `Cohort Average: ${document.getElementById('cohort-avg').textContent}\n\n`;
  reportContent += 'Per-student summaries:\n';
  reportContent += '-----------------------\n';

  allStudents.forEach(student => {
    const marks = (student.marks || []).join(';');
    reportContent += `${student.sid},${student.name},${student.average.toFixed(1)},${student.max},${student.min},${student.grade}\n`;
  });

  const blob = new Blob([reportContent], { type: 'text/plain' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = `student-report-${Date.now()}.txt`;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

// Main refresh function
async function refresh() {
  try {
    // Fetch data
    allStudents = await fetchStudents();
    const report = await fetchReport();

    // Reset search and sort
    document.getElementById('search-input').value = '';
    filteredStudents = [...allStudents];
    currentSort = { column: null, ascending: true };

    // Update UI
    renderStudents(filteredStudents);
    
    if (report) {
      updateStats(allStudents, report);
      updateGradeDistribution(report);
    }

    // Clear errors
    document.getElementById('error-container').innerHTML = '';
  } catch (error) {
    showError('Failed to refresh data: ' + error.message);
  }
}

// Tab switching
function setupTabs() {
  const tabBtns = document.querySelectorAll('.tab-btn');
  tabBtns.forEach(btn => {
    btn.addEventListener('click', () => {
      // Hide all tabs
      document.querySelectorAll('.tab-content').forEach(tab => {
        tab.classList.remove('active');
      });

      // Remove active class from all buttons
      tabBtns.forEach(b => b.classList.remove('active'));

      // Show selected tab and mark button as active
      const tabId = btn.getAttribute('data-tab');
      document.getElementById(tabId).classList.add('active');
      btn.classList.add('active');
    });
  });
}

// Event listeners
document.addEventListener('DOMContentLoaded', () => {
  setupTabs();
  
  document.getElementById('refresh-btn').addEventListener('click', refresh);
  document.getElementById('search-input').addEventListener('input', filterStudents);
  document.getElementById('export-btn').addEventListener('click', exportReport);

  // Load data on page load
  refresh();
});

