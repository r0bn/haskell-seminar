#! octave -qf
r=1000;                                 # Number of Rows (max 10000)
c=1000;                                 # Number of Columns (max 10000)
file="matrix.csv";                     # Filename of output file
wantSolve=true;
#method="Ones matrix"
#method="Standard distribution"
method="Normal distribution"
#method="Chebyshev spectral differentiation matrix"
#method="Simple chemical system"

# Generate Matrix and solution vector
switch (method)
    case "Ones matrix"
        A = ones (r, c);
        b = ones (r, 1);
    case "Standard distribution"
        A = stdnormal_rnd(r, c);
        b = stdnormal_rnd(r, 1);
    case "Normal distribution"
        mean_mu = 1000.0;
        sigma_stddev = mean_mu / 2.0;
        A = normrnd (mean_mu, sigma_stddev, r, c);
        b = normrnd (mean_mu, sigma_stddev, r, 1);
    case "Chebyshev spectral differentiation matrix"
        A = gallery ("chebspec", r, 1 );
        b = ones (r, 1);
    case "Simple chemical system"
        r = 2;
        c = 2;
        A = [ 2, 0; 0, 2 ];
        b = [ 2; 1 ];
    otherwise
        error ("Unavailable method selected")
endswitch

# Solve
if (wantSolve)
    disp ("Solving")
    x = A \ b;
    disp ("Solution: ")
    disp (x)
endif

disp ("Writing file")
fid = fopen (file, "w");
fprintf(fid, "# Rows\n%d\n", r);
fprintf(fid, "# Columns\n%d\n", c);
fclose (fid);
dlmwrite (file, A, ",", "append", "on");
dlmwrite (file, b, ",", "append", "on");
